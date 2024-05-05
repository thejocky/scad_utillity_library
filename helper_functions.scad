
// Returns true if paramater is of type range
function is_range(x) = is_num(x[0]) && !is_list(x);

// Rotate point around location(default to [0,0]) by angle degrees
function rotatePoint(point, angle, location=[0,0]) = 
    let(d = sqrt((point[0]-location[0])^2+(point[1]-location[1])^2),
        a = atan2(point[1]-location[1],point[0]-location[0]))
    [cos(a+angle)*d+location[0],
    sin(a+angle)*d+location[1]];

// Find distance between two points
function distance(pointA, pointB) =
    sqrt((pointA[0]-pointB[0])^2 + (pointA[1]-pointB[1])^2);

// Point between pointA and pointB
// Bias of 0 returns pointA, 1 returns pointB, 0.5 return half way between
function midPoint(pointA, pointB, bias = 0.5) =
    [(pointA.x*(1-bias) + pointB.x*bias), (pointA.y*(1-bias) + pointB.y*bias)];

// Returns angle from pointA to pointB
function angle(pointA, pointB) = 
    atan2(pointB[1]-pointA[1], pointB[0]-pointA[0]);

// Returns gradiant from 0-1
// Power goes from constant gradiant to step from 0 to 1 at mid point
function gradiant(power=1) = function(n)
    (n < 0.5)?
        min((2*n)^power / 2, 1):
        max(-(-2*n+2)^power / 2 + 1, 0);

// Flattens a mixed set of lists
function flatten(l) =
    [for (i=l) each is_list(i)? flatten(i):i];

function flattend(l, d=2) =
    let(worker = function(cl, cd)
        [for (i=cl) (is_list(i))?
            (cd>=d)? flatten(i):
                worker(i, cd+1):
            i]) worker(l, 2);
