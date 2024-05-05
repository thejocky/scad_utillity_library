







// transformations ------------------------------------------------------------

function dialate(xScale, yScale, origin=[0,0]) =
    ["d",
        function(xVal, centered=true) 
            centered? (xVal-origin.x)*xScale+origin.x:
                      xVal*xScale,
        function(yVal, centered=true)
            centered? (yVal-origin.y)*yScale+origin.y:
                      yVal*yScale,
        function(point) [(point.x-origin.x)*xScale+origin.x,
                            (point.y-origin.y)*yScale+origin.y]];


function translate(xOffset, yOffset) =
    ["t", 
        function(x, centered=true) x+xOffset,
        function(y, centered=true) y+yOffset,
        function(point) point+[xOffset, yOffset]];

function rotate(angle, origin=[0,0]) =
    ["r",
        function(point)
            let(d=distance(point, origin),
                a=angle(origin,point))
                    origin + [cos(angle+a)*d, sin(angle+a)*d],
        function(a) a+angle];

// Base curves / lines --------------------------------------------------------

function curveModel(curve=function(n)[0,0], finalizer=base_finalizer,
    dialate=undef, translate=undef, rotate=undef, defaultTransform=undef) =
        function(operation, arg)
            (operation == "finalize")? finalizer(curve):
            (operation == "getFunction")? curve:
            (operation == "transform")?
                (arg[0] == "d")?
                    (is_undef(dialate))?
                        defaultTransform(arg):
                        dialate(arg):
                (arg[0] == "t")?
                    (is_undef(translate))?
                        defaultTransform(arg):
                        translate(arg):
                (arg[0] == "r")?
                    (is_undef(rotate))? 
                        defaultTransform(arg):
                        rotate(arg):
                defaultTransform(arg):
            undef;


arc = function (r, b, p=[0,0]) curveModel(
    curve = function(n)
        [p[0]+(cos(b[0]+(b[1]-b[0])*n)*r),
        p[1]+(sin(b[0]+(b[1]-b[0])*n)*r)],
    finalizer = base_finalizer,

    dialate = function(t)
        elipticArc(t[1](r), t[2](r), b, t[3](p)),
    translate = function(t)
        arc(r, b, t[3](p)),
    rotate = function(t)
        arc(r, [t[2](b[0]),t[2](b[1])], t[1](p)),
        
    defaultTransform = function(t)
        elipticArc(t[1](r,true), t[2](r,true), 
            [t[4](b[0]),t[4](b[1])], t[3](p))
);

elipticArc = function(rx, ry, b, p=[0,0], a=0) curveModel(
    curve = function(n)
        let (tb = b - [a, a])
        rotatePoint([(cos(tb[0]+(tb[1]-tb[0])*n)*rx),
        (sin(tb[0]+(tb[1]-tb[0])*n)*ry)], a) + p,
    finalizer = base_finalizer,

    dialate = function(t)
        elipticArc(t[1](rx), t[2](ry), b, t[3](p), a),
    translate = function(t)
        elipticArc(t[1](rx), t[2](ry), b, t[3](p), a),
    rotate = function(t)
        elipticArc(rx, ry,  [t[2](b[0]),t[2](b[1])], t[1](p), t[2](a)),

    defaultTransform = function(t)
        elipticArc(t[1](rx,false), t[2](ry,false), 
            [t[4](b[0]),t[4](b[1])], t[3](p))
);

line = function (pA, pB) curveModel(
    curve = function(n)
        [pA[0] + n*(pB[0]-pA[0]),
        pA[1] + n*(pB[1]-pA[1])],
    finalizer = edge_finalizer,

    rotate = function(t)
        line(t[1](pA), t[1](pB)),
    defaultTransform = function(t)
        
        line(t[3](pA), t[3](pB))
);

point = function (p) curveModel(
    curve = function(n) p,
    finalizer = point_finalizer,
    rotate = function(t) point(t[1](p)),
    defaultTransform = function(t) point(t[3](p))
);


// Curve operations -----------------------------------------------------------

function curveOperationModel(baseCurves=[], selfReference=undef, 
    operation=function(n)[], finalizer=base_finalizer, dialate=undef,
    translate=undef, rotate=undef, defaultTransform=undef) =
    let(convertToFuncs = function(curves)
        [for (c=curves) c("getFunction")])

        function(op, arg)
            (op == "finalize")?
                // echo("finalizing curve Operation:", selfReference)
                finalizer(operation(convertToFuncs(baseCurves))):
            (op == "getFunction")?
                operation(convertToFuncs(baseCurves)):

            (op == "transform")?
                (arg[0] == "d")?
                    (is_undef(dialate))?
                        selfReference([for(c=baseCurves)c(op, arg)]):
                        dialate(arg):
                (arg[0] == "t")?
                    (is_undef(translate))?
                        selfReference([for(c=baseCurves)c(op, arg)]):
                        translate(arg):
                (arg[0] == "r")?
                    (is_undef(rotate))? 
                        selfReference([for(c=baseCurves)c(op, arg)]): 
                        rotate(arg):
                (is_undef(defaultTransform))?
                    defaultTransform(arg):
                selfReference([for(c=baseCurves)c(op, arg)]):
            undef;


function weightedAverage(w) = function(a, b) curveOperationModel(
    baseCurves = [a,b],
    selfReference = function(f) weightedAverage(w)(f[0],f[1]),
    operation = function(weightedAverageFuncs) function(n)
        // echo("weightedAverage:", w(n), weightedAverageFuncs[1](n)*w(n), weightedAverageFuncs[1](n))
        weightedAverageFuncs[0](n)*(1-w(n)) + weightedAverageFuncs[1](n)*w(n)
);

function curveSection(range) = function (curve) curveOperationModel (
    baseCurves = [curve],
    selfReference = function(f) curveSection(range)(f[0]),
    operation = function(curveSectionFuncs) function(n)
        // echo("curveSection:", f[0](n*(range[1]-range[0])+range[0]))
        curveSectionFuncs[0](n*(range[1]-range[0])+range[0])
);


// Transforms all given curves
// Transforms and curves can be in and out of a list
// transformCurves(transforms)(curves)
function transformCurves(transforms) =
    let(t = (!is_list(transforms[0]))? [transforms]:transforms)
    modify_model(t,
        function(i,t,c) sequential_model(t,
            function (i,t,c2) c2("transform", t)
        )(c)
    );

// Returns list of original curve and curve after applying transforms
// Transforms and curves can be in and out of a list
// copyCurves(transforms)(curves)
function copyCurves(transforms) =
    let(t = (!is_list(transforms))? [transforms]: transforms)
    modify_model(t,function(i,t,c)
        [c, transformCurves(t)(c)]
    );

// Returns list of copies of the curves after each transformation
// initial determins if original curve is added to list
// nested lists of transforms are handled as single transform returning one copy
// Transforms and curves can be in and out of a list
// seqCopyCurves(transforms, initial=true)(curves)
function seqCopyCurves(transforms, initial=true) =
    let(t = (!is_list(transforms))? [transforms]: transforms)
    modify_model(t,function(i,t,c)
        seqCopy_model(t, initial=initial,
        forEach = function (i,t,c)
            is_list(t[0])?sequential_model(t, 
                function(i,t,c) c("transform", t)
            )(c): c("transform", t)
        )(c)
    );

// Rotates and copies curves across origin
// List of curves are flattened before modification
// polarCopy(copies, direction, origin) (curves)
function polarCopyCurve(copies, d=STD_CW, o=[0,0]) =
    copy_model ([0:copies-1], function(i,ci,c)
        let(curves = is_list(c[0])? flatten(c):c)
        modify_model(ci,function(i,t,c)
            c("transform", rotate(360/copies*ci*d, o))
        )(curves) 
    );




// Line finalizers ------------------------------------------------------------

// points corrisponding to 0-1 inclusive on function
// for f total points
base_finalizer = function (func)
    [for (i=[0:$fn-1]) func(i/($fn-1))];

// Returns 2 points corrosponding to 0 and 1 in function
edge_finalizer = function(func)
    [func(0), func(1)];

// Returns 1 point corrosponding 
point_finalizer = function(func)
    [func(0.5)];



// Finalizer Functions --------------------------------------------------------


// finalize arg by converting into vector of points (defaults to $fn points)
// Note: for multiple arguments pass array
finalize = function (arg, f=$fn)
    (is_function(arg))?
        [for (i=[0:f-1])
            arg(i/(f-1), f)
        ]
    :(is_list(arg))?
        [for (i=arg) each
            finalize(i, f)
        ]
    :undef;

module finalize(objects) {
    if (is_list(objects[0])) {
        for (i=objects) {
            finalize(i);
        }
    } else {

        if (objects[0] == "shape") {
            shape_finalizer(objects);
        } else if (objects[0] == "circle") {
            circle_finalizer(objects);
        } else if (objects[0] == "difference") {
            difference_operation(objects);
        } else if (objects[0] == "rotate") {
            rotate_operation(objects);
        } else if (objects[0] == "linear_extrude") {
            linear_extrude_operation(objects);
        } else if (objects[0] == "polygon") {
            polygon_finalizer(objects);
        } else if (objects[0] == "square") {
            square_finalizer(objects);
        } else if (objects[0] == "sphere") {
            sphere_finalizer(objects);
        } else if (objects[0] == "polarCopy") {
            polarCopy_operation(objects);
        } else if (objects[0] == "translate") {
            translate_operation(objects);
        }
    }
}

function toPoints(c) =
    (!is_list(c))?
        c("finalize"):
        [for (curve=c) each toPoints(curve)];


// Shape Finalizers ---------------------------------------


// 2D Shape -------------------------------------

// Mark list as shape operation
// shape opertation creates polygon from given lines
shape = function (arg)
    (is_list(arg))?
        concat("shape", arg)
    :(is_function(arg))?
        ["shape", arg]
    :undef;

module shape_finalizer(args) {
    polygon ([for (c = flatten(args)) each
        is_function(c)?
            c("finalize"):
        undef]);
}

polygon = function (points, paths=undef)
    ["polygon", points, paths];

module polygon_finalizer(args) {
    polygon(args[1], args[2]);
}

circle = function (r, location = [0,0])
    ["circle", r, location];

module circle_finalizer(args) {
    translate(args[2])
    circle(args[1]);
}

square = function (size, center=false)
    ["square", size, center];

module square_finalizer(args) {
    square(args[1], args[2]);
}


// 3D Shapes ------------------------------------

sphere = function (radius, o=[0,0,0])
    ["sphere", radius, o];
module sphere_finalizer(args) {
    translate(args[2])
    sphere(args[1]);
}


// Finalizer Operations -----------------------------------

// Mark given shapes for difference opperation
// uses difference module on shapes defined by arg
difference = function() function (args, f=$fn)
    (is_list(args))?
        concat("difference", args)
    :(is_function(args))?
        ["difference", args]
    :undef;

module difference_operation(args, f=$fn) {
    difference() {
        finalize(args[1]);

        if (len(args) > 2) {
            for (i = [2:len(args)-1]) {
                finalize(args[i]);
            }
        }
    }
}

// Mark given shapes for 'rotate' operation
// Function wraper for 'rotate', storing parameters in array
// format: rotatef([x,y,z]) or rotate(a, [x,y,z])
rotatef = function(arg1, arg2) function(objects)
    ["rotate", arg1, arg2, objects];
module rotate_operation(args, f=$fn) {
    
    if (is_undef(args[2])) {
        if (len(args) > 3) {
            rotate(args[1])
            finalize(args[3]);
        }
    }
}

// Mark given shapes for 'linear_extrude' operation
// Function wraper for 'linear_extrude', storing parameters in array
linear_extrude = function(height = 5, center = true, convexity = 10,
        twist = 0, slices = 20, scale = 1.0) function(objects)
    ["linear_extrude", height, center, convexity, twist, slices, scale, objects];

module linear_extrude_operation(args, f=$fn) {
    linear_extrude(height=args[1], center=args[2], convexity=args[3],
        twist=args[4], slices=args[5], scale=args[6]){
        finalize(args[7]);
    }
}

// Mark given shapes for 'translate' operation
// Function wraper for 'translate', storing parameters in array
// Format: translate([x,y,z]) {...}
translate_obj = function(v) function(objects)
    ["translate", v, objects];

module translate_operation(args, fn=$fn) {
    translate(args[1])
    finalize(args[2]);
}

// rotate_obj = function()

// Polar copies object n iteration rotation around point o
// Only currently supported around z axis
polarCopyObject = function(copies, d=STD_CW, o=[0,0]) function(objects)
    ["polarCopy", copies, d, o, objects];
module polarCopy_operation(args, fn=$fn) {
    for (i=[0:args[1]-1]) {
        translate([args[3].x, args[3].y, 0])
        rotate([0,0,i*360/args[1] * args[2]]) 
        translate([-args[3].x, -args[3].y, 0]) finalize(args[4]);
    }

}


// // Base shapes



// $fn=100;


// function modifyTest(transforms) = modify_model(transforms,
//     function (i, m, c) c("transform", m)
// );

// function copyTest(transforms) = copy_model(transforms,
//     function (i, m, c) c("transform", m)
// );

// function sequenceTest(transforms) = sequential_model(
//     transforms, function (i, m, c) c("transform", m)
// );

// function seqCopyTest(transforms) = seqCopy_model(
//     transforms, function (i, m, c) c("transform", m)
// );


// Util functions using shaping

// Connects two circles by two tangental lines
// Circle is list [[x, y], rad]
function connectCircles(cA, cB) =
    let(dist = distance(cA[0], cB[0]), // Distance between points
        angle = angle(cA[0], cB[0]),
        // tDist = sqrt(dist^2 (cA[1]-cB[1])^2), // Distance of tangent line
        tAngle = asin((cB[1]-cA[1])/dist) // angle of the tangent reltivie to angle 
        )
    [shape([
        arc(cA[1], [angle+90+tAngle-360, angle-90-tAngle], cA[0]),
        arc(cB[1], [angle-90-tAngle, angle+90+tAngle], cB[0])
    ])];

