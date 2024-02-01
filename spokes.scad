include <std_functions.scad>

spoke = function(count, spokeWidth, rimWidth, centerHole = undef)
    function(r)
    let(innerWidth = is_num(rimWidth)? rimWidth : rimWidth[0],
        outerWidth = is_num(rimWidth)? rimWidth : rimWidth[1],
        innerRad = is_num(centerHole)? centerHole+innerWidth: innerWidth,
        outerRad = r - outerWidth
    )[ // Create spoke shape
        difference() ([
            circle(outerRad),
            circle(innerRad),

            // shape(polarCopy(count, STD_CCW) ([
            //     line([spokeWidth/2,r],[spokeWidth/2,0]),
            //     line([-spokeWidth/2,0],[-spokeWidth/2,r])
            // ])),
            for (i=[0:count-1])
                rotatef([0,0,360/count*i]) ([
                    shape([
                        line([spokeWidth/2,r],[spokeWidth/2,0]),
                        line([-spokeWidth/2,0],[-spokeWidth/2,r])
                    ])
                ]),
            
        ]),
        is_num(centerHole)? circle(centerHole) : ["pass"],
        
    ];
        

// Spoke with only outer rim and no connection to center
rim_spoke = function(rimWidth) function (r)
    circle(r-rimWidth);


// ratchet spokes
ratchetSpokeOuter = function(teethCount, toothLength, outerRim)
    function(r)
    let(pointA = [r-outerRim,0],
        pointB = rotatePoint([r-outerRim-toothLength, 0], 360/(2*teethCount)))
        echo(pointA, pointB)
    shape([polarCopyCurve(teethCount) (line(pointB, pointA))]);


centerHole_spoke = function(centerRad)
    function (r) circle(centerRad);

squareHole_spoke = function(sideLen)
    function (r) square(sideLen, center=true);