// Allows creating of function wrappers one or more targets and one or more modifiers,
// linking them using a provided function.
// Modifier can be anything that defines how to modify the target, a function, a value, or something else
//
// forEach(i, m, t) function takes 3 paramaters,
// i : First parameter is the current index, of the model, starting at zero and incrementing each time the 
//      function is called
// m : Second parameter is the current modifier,
// t : Third Parameter is the current target


// Examples provided will be based on following constants and functions
// UP, DOWN, LEFT, RIGHT - constants for directions
// move(point, dir, dist) - returns the point shifted in direction dir by dist steps

function is_range(x) = is_num(x[0]) && !is_list(x);


// modify_model: applies modifier on each target
//  takes in one or more targets and one modifier then calls 
//      forEach function with each target, returns list each output from forEach
// Usage of mod isn't strictly nessisary for this model as it remains constant, but is left
//      in for consistancy with all models

// eg.
//  function shiftPoints(points, dir, dist) =
//      modify_model(
//          dir,
//          forEach = function(i, m, t) move(t, m, dist)
//      ) (points);
// 
// input = [ [0,0], [1,2], [-3,1] ];
// output = shiftPoints(input, UP, dist);
// echo(output);
// ECHO: "[ [0,1], [1,3], [-3,2] ]"

// target mod  result
// t0 --- m0 - r0
// t1 --- m0 - r1
// t2 --- m0 - r2
// t3 --- m0 - r3

function modify_model(mod=undef, forEach) = function(targets)
    let(t = (!is_list(targets))? [targets]:targets)
    [for (i=[0:len(t)-1]) forEach(i, mod, t[i])];




// copy_model: Creates a copies of target, each with different modifier applied
//  Takes in one target and one or more modifiers, runs forEach once with each modifier
//      the model then returns a list of output from each call to forEach
//  copy_model is able to take range for mod, internaly expanded to list containing output of range
// Usage of target isn't strictly nessisary for this model as it remains constant, but can be
//      used to return a function that then takes a target
//
// eg.
//  function copyPoint(copyOffsets) =
//      copy_model(
//          copyOffsets,
//          forEach = function(i, m, t) (t, m[0], m[1])
//      );
// 
// copyOffsets = [ [UP,1], [DOWN,10], [LEFT,3] ];
// pointCopier = copyPoint(copyOffsets)
// output = pointCopier([0,0]);
// echo(output);
// ECHO: "[ [0,1], [0,-10], [-3,0] ]"

// target mod  result
// t0 -|- m0 - r0
//     |- m1 - r1
//     |- m2 - r2
//     |- m3 - r3

function copy_model(mods, forEach) = function(target)
    let(m = is_range(mods)? [for (i=mods) i]:
        is_list(mods)? mods: [mods])
    [for (i=[0:len(m)-1]) forEach(i, m[i], target)];




// sequential_model: Applies each mod to target in sequence
//  Takes in one target and one or more modifiers, runs forEach once with each modifier,
//      and after the first call the input target is the output of the previous call to forEach
//      the model then returns the final forEach call return.
//  sequential_model is able to take range for mod, internaly expanded to list containing output of range
//
// eg.
//  function movePoint(point, steps) =
//      sequential_model(
//          steps,
//          forEach = function(i, m, t) move(t, m[0], m[1])
//      )(point);
// 
// steps = [ [UP,1], [DOWN,10], [LEFT,3], [LEFT,5] ];
// output = movePoint([0,0], steps);
// echo(output);
// ECHO: "[-8,-9]"

// target mod  result
// t0 --- m0
//        !
//        m1
//        !
//        m2
//        !
//        m3 - r

function sequential_model(mods, forEach) = function(target)
    let(m = is_range(mods)? [for (i=mods) i]:
        is_list(mods)? mods: [mods])
    let(worker = function(i=0, target)
        (i==len(m))? target:
        worker(i+1, forEach(i, mods[i], target)))
    worker(0, target);
    



// seqCopy_model: returns copy of curve each time a mod is applied in sequence
//  Takes in one target and one or more modifiers, runs forEach once with each modifier,
//      and after the first call the input target is the output of the previous call to forEach
//      the model returns the output of each call to forEach, aswell as the original target if
//      if initial is set to true.
//  sequential_model is able to take range for mod, internaly expanded to list containing output of range
//
// eg.
//  function copyPoint(point, steps) =
//      sequential_model(
//          steps,
//          forEach = function(i, m, t) move(t, m[0], m[1]),
//          initial=true
//      )(point);
// 
// steps = [ [UP,1], [DOWN,10], [LEFT,3], [LEFT,5] ];
// output = movePoint([0,0], steps);
// echo(output);
// ECHO: "[ [0,0], [0,1], [0,-9], [-3, -9], [-8,-9] ]"
// 
// initial determins if original curve is added to list

// target mod  result
// t0 -------- r? (first element if initial == true)
//        !
//        m0 - r0
//        !
//        m1 - r1
//        !
//        m2 - r2
//        !
//        m3 - r3

function seqCopy_model(mods, forEach, initial=true) = function(target)
    let(m = is_range(mods)? [for (i=mods) i]:
        is_list(mods)? mods: [mods])
    let(worker = function(i=0, c)
        (i==len(m))? c:
        concat([c],
                worker(i+1, forEach(i, m[i], c))))     
    initial? worker(0, target):
        worker(1, forEach(0, m[0], target));


// function foldl_model(mod, forEach, base) =
//     let(t = (!is_list(targets))? [targets]:targets)
//     [for (i=[0:len(t)-1]) forEach(i, mod, t[i])];


// Generic Modifers -------------------------------------------------

// serialOperation(operations)(target)
// operations - singler or list of functions to 
function serialOperation(operations) =
    let(o = (!is_list(operations))? [operations]:operations)
    sequential_model(
        o, 
        forEach = function(i,o,targets) o(targets)
    );

// map(operations)(targets)
// Maps function to all targets, multiple functions can be provided and
//   will be applied in a serial mannor with the output of each being the input of the next
function map(operations) = 
    let(o = is_list(operations)? nestedOps(operations) : operations)
    modify_model(
        forEach = function(i, _, t) o(t)
    );

// nestedOps(operations) (target)
// Given list of functions, creates lamda which executed each one serially on target
// given array [f1 f2 f3 f3] unravels to f4( f3( f2( f1 (target) ) ) )
function nestedOps(operations) =
    sequential_model(
        mods = operations,
        forEach = function(i, m, t) m(t)
    );


function foldl(op, base, data) =
    sequential_model(
        mods = data,
        forEach = function(i, m, t) op(m, t)
    )(base);

function filter(cond, data) =
    foldl(
        function(element, acc)
            cond(element)?
                concat([element], acc)
            :   acc,
        [],
        data
    );


function qsort(comp, data) =
    let(check = function(r) function(l) comp(l, r),
        check_not = function(r) function(l) !comp(l, r),
        pivot = data[0])
    (len(data) <= 1)?
        data
    : echo(pivot, len(data))   
        concat(
            qsort(comp, filter(check(pivot), data[1:len(data)])),
            qsort(comp, filter(check_not(pivot), data[1:len(data)]))
    );

la = [3,4,2,5,7,4,6];
qsort(function(lv, rv) lv<rv, [3,4,2,5,7,4,6]);
echo(qsort(function(lv, rv) lv>rv, [3,4,2,5,7,4,6]));
echo(qsort(function(lv, rv) lv==rv, [3,4,2,5,7,4,6]));


