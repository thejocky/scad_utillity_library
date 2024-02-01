include <std_functions.scad>
include <spokes.scad>

// Basic Trig functions using radians rather than degrees

RAD = PI/180;
DEG = 180/PI;

function cosr(a) = cos(a*DEG);
function sinr(a) = sin(a*DEG);
function tanr(a) = tan(a*DEG);

function gear_pitchRad(m,z,p=20) = m*z/2;
function gear_adendum(m,z,p=20) = gear_pitchRad(m,z,p) + m;
function gear_dedendum(m,z,p=20) =
    let(clearance = m*0.2) (m*(z-2)-2*clearance)/2;


// Generates 2d outline of involute curve
// m - module size
// z - tooth count
// p - pitch outline
function involuteGear_outline(m, z, p=20, spoked=undef) =
    let(clearance = m*0.2,

        // Reference circle's diameters
        PD = m*z,                    // Pitch Circle Diameter
        BD = PD*cos(p),              // Base Circle Diameter
        AD = PD + 2*m,               // Addendum Circle Diameter
        DD = m*(z-2)-2*clearance,    // Dedendum Circle Diameter


        // Reference circle's radii
        PR = PD/2,                   // Pitch Circle Radius
        BR = BD/2,                   // Base Circle Radius
        AR = AD/2,                   // Addendum Circle Radius
        DR = DD/2,                   // Dedendum Circle Radius


        // Base width of tooth on base circle (radians)
        toothWidth = 2*(m/PD * PI/2 + tan(p)-p*RAD),

        // Angle of point forming dedendum, distance is DR (degrees)
        rootAngle = let(angle = (2*PI/z - toothWidth)/2)
            toothWidth/2 - asin((sinr(angle)*cosr(angle)*(BR-DR)) / DR) * RAD,

        // 2*PI/z - 2m/PD + PI + 2tan(p) - 2*p*RAD

        // Angle, distance, and radious of circle defining root of gear 
        rootCircle =
            let(
                angle = (2*PI/z - toothWidth)/2,
                dist = cosr(angle)*max(BR,DR),
                rad = sinr(angle)*max(BR,DR),
                createRoot = (BR < DR)? false: true)
            [angle, dist, rad, createRoot],

        // Angles of involute curve needed between base and adendum
        
        angles = 
            let(a = function(dist, BD) sqrt(pow(dist,2) - pow(BD,2)) / BD,
                d = max(AR-BR,DR))
            [for (i = [max(BR,DR):min(AR-BR,AR-DR)/$fn:AR+1/(AR*$fn)]) a(i, BR)],

        
        // Intermediary functions

        // Creating involute curves
        involLeft = function(radius, angle, width, o)
            [(sinr(angle - width/2 + o)*angle + cosr(angle - width/2 + o)) * radius, 
            (-cosr(angle - width/2 + o)*angle + sinr(angle - width/2 + o)) * radius],
        involRight = function(radius, angle, width, o)
            [(sinr(angle - width/2 + o)*angle + cosr(angle - width/2 + o)) * radius, 
            (cosr(angle - width/2 + o)*angle - sinr(angle - width/2 + o)) * radius],

        // Creating list of points representing one tooth
        tooth = function(offset)
            concat([],
                rootCircle[3]?
                    let(angle = (offset-rootCircle[0])*DEG)
                    // transformCurves([translate(0,0)]) (
                        elipticArc(rootCircle[2],rootCircle[2], [270,90], [rootCircle[1]-(max((BR-DR-rootCircle[2]),0)),0])
                    ("transform", rotate(angle-toothWidth*DEG/2))("finalize")
                
                :   [],
                

            [for (i = [0:$fn]) involLeft(BR, angles[i], toothWidth, offset),
            for (i = [$fn:-1:0]) involRight(BR, angles[i], toothWidth, -offset)]
            // [cosr(offset+rootAngle)*DR,sinr(offset+rootAngle)*DR]
            )

    ) difference() ([ 
        polygon([for (i=[0:z-1]) each
            (tooth(2*PI/z * i))]),
        (!is_undef(spoked))? spoked(DR) : []
    ]);







// $fn = 100;

// rotate = 0;

// gear1 = [.5, 70, 20];
// gear2 = [.5, 70, 20];


// // translate([-gear_pitchRad(0.5,60),0])
// linear_extrude(1)
// color ("aqua") union() {
//     rotate ([0, 0, $t * 360/gear1[1]]) // $t * 360/gear1[1]
//         GearOutline(gear1[0], gear1[1], gear1[2]);

//         rotate([0,0,180/gear1[1]])
//             circle((gear1[0]*gear1[1]*cos(gear1[2]))/2);
// }




// // linear_extrude(1)
// // color ("aqua") union() {
// //     rotate ([0, 0, $t * 360/gear1[1]]) // $t * 360/gear1[1]
        
// //         finalize(involuteGear_outline(gear1[0], gear1[1], gear1[2]));

// //         // rotate([0,0,180/gear1[1]])
// //         //     circle((gear1[0]*gear1[1]*cos(gear1[2]))/2);
// // }

// translate([gear1[0]*gear1[1]/2 + gear2[0]*gear2[1]/2, 0, 0])
//     rotate ([0, 0, -$t * 360/gear2[1] + (gear2[1]%2 - 1) * 180/gear2[1]]) // 
//         finalize(involuteGear_outline(gear2[0], gear2[1], gear2[2]));




// translate([gear1[0]*gear1[1]/2 + gear2[0]*gear2[1]/2, 0, 0])
//     rotate ([0, 0, 360/gear2[1]])
//         rotate ([0, 0, -$t * 360/gear2[1]])
//             GearOutline(gear2[0], gear2[1], gear2[2]);

// color ("pink")
//     linear_extrude(0.1)
//         rotate([0,0,180/gear1[1]])
//             circle((gear1[0]*gear1[1]*cos(gear1[2]))/2);

// color ("hotpink")
//     linear_extrude(0.1)
//         translate([gear1[0]*gear1[1]/2 + gear2[0]*gear2[1]/2, 0, 0])
//             // circle((gear2[0]*gear2[1]*cos(gear2[2]))/2);
//             circle((gear2[0]*(gear2[1]-2)-2*gear2[0]*0.2)/2);

// echo (gear2[0]*(gear2[1]-2)-2*gear2[1]*0.2);




// Creating showcase for gears that will be used in clock

// mSize = 0.5;
// pitch = 20;
// z1 = 60;
// z2 = 70;
// gear1Rad = gear_pitchRad(mSize, z1);
// gear2Rad = gear_pitchRad(mSize, z2);


// echo("Gear rad: ", gear1Rad);

// axleEndR =  1;
// axleR = 1.5;

// holderWidth = 2;

// gearWidth = 2;
// axleLength = 5 + 2*holderWidth;
// gearClearence = 0.3;
// vertClearance = 1;

// mods = [0.55, 0.56, 0.521, 0.48];
// gearZ = [96, 80, 75, 80];
// pinionZ = [0, 12, 10, 10];


// module GearAndPinion(Gmod, Pmod, width, pinionWidth, gearZ, pinionZ, spoke=undef) {
//     linear_extrude(width)
//         finalize(involuteGear_outline(Gmod, gearZ, pitch, spoke));
//     color("red")
//     linear_extrude(width + pinionWidth)
//         finalize(involuteGear_outline(Pmod, pinionZ, pitch));
// }

// module GearShaft(len, rad, endRad, endLen, midLen) {
//     linear_extrude(len) circle(endRad);
    
//     translate([0,0,endLen])
//     linear_extrude((len-midLen)/2 - endLen, scale=rad/endRad)
//     circle(endRad);

//     translate([0,0,(len-midLen)/2])
//     linear_extrude(midLen)
//     circle(rad);

//     translate([0,0,len-(len-midLen)/2])
//     linear_extrude((len-midLen)/2 - endLen, scale=endRad/rad)
//     circle(rad);
// }

// module GearTrain(m, Gz, Pz, gearWidth, gearClearance, vertClearance) {

//     pitch = 20;

//     holderWidth = 7;

//     bearingClearence = 0.4;
//     axleRad = 1.5;
//     axleEndRad = 0.75;
//     axleEndLen = 5 + bearingClearence/2;
//     chamferLen = 2.25;
//     pivotClearence = 0.15;

//     gearSpoke4 = spoke(4, 3, [4,3]);
//     gearSpoke5 = spoke(5, 3, [4,3]);

//     axleLen = axleEndLen*2 + chamferLen*2 + 
//                 gearWidth*3 + vertClearance*4;
//     axleMidLen = gearWidth*3 + vertClearance*4;

//     Grad = [gear_pitchRad(m[0], Gz[0]) + gearClearance/2,
//             gear_pitchRad(m[1], Gz[1]) + gearClearance/2,
//             gear_pitchRad(m[2], Gz[2]) + gearClearance/2, 
//             gear_pitchRad(m[3], Gz[3]) + gearClearance/2];

//     Prad = [0, gear_pitchRad(m[0], Pz[1]) + gearClearance/2,
//                     gear_pitchRad(m[1], Pz[2]) + gearClearance/2,
//                     gear_pitchRad(m[2], Pz[3]) + gearClearance/2];
    
//     // // Barrel
//     union() {
//         translate([0,0,-gearWidth/2])
//         linear_extrude(gearWidth)
//         finalize(involuteGear_outline(m[0], Gz[0], pitch, gearSpoke4));
//         translate([0,0,-axleLen/2])
//         GearShaft(axleLen, axleRad, axleEndRad,
//                     axleEndLen, axleMidLen);
//     }

//     // Center
//     centerX = Grad[0]+Prad[1];
//     union() {
//         translate([centerX,0, 1.5*gearWidth+vertClearance])
//         rotate([0,180,3*180/Pz[1]])
//         GearAndPinion(m[1], m[0], gearWidth, gearWidth*2+vertClearance*3, Gz[1], Pz[1], gearSpoke5);
//         translate([centerX,0,-axleLen/2])
//         GearShaft(axleLen, axleRad, axleEndRad,
//                     axleEndLen, axleMidLen);
//     }

//     // Third
//     ThirdX = centerX + Grad[1]+Prad[2];
//     union () {
//         translate([ThirdX,0, -gearWidth/2])
//         rotate([0,0,180/Pz[2]])
//         GearAndPinion(m[2], m[1], gearWidth, gearWidth+vertClearance*2, Gz[2], Pz[2], gearSpoke4);
//         translate([ThirdX,0,-axleLen/2])
//         GearShaft(axleLen, axleRad, axleEndRad,
//                     axleEndLen, axleMidLen);
//     }

//     // Fourth
//     FourthX = ThirdX + Grad[2]+Prad[3];
//     union () {
//         translate([FourthX,0, -gearWidth*1.5 - vertClearance])
//         rotate([0,0,-Gz[2]/Pz[3]])
//         GearAndPinion(m[3], m[2], gearWidth, gearWidth*2+vertClearance*3, Gz[3], Pz[3], gearSpoke5);
//         translate([FourthX,0,-axleLen/2])
//         GearShaft(axleLen, axleRad, axleEndRad,
//                     axleEndLen, axleMidLen);
//     }

//     // holder

//     module holderOutline () {
//         finalize([difference() ([
//             shape([
//                 arc(6, [-90, 90], [FourthX + (Grad[3]+7), 0]),
//                 arc(6, [90, 270], [-(Grad[0]+7), 0]),
//             ]),
//             circle(axleEndRad+pivotClearence, [FourthX, 0]),
//             circle(axleEndRad+pivotClearence, [ThirdX, 0]),
//             circle(axleEndRad+pivotClearence, [centerX, 0]),
//             circle(axleEndRad+pivotClearence, [0, 0]),

//         ])]);
//     }


//     // module holderOutline () {
//     //     finalize([difference() ([
//     //         shape([
//     //             arc(6, [-90, 90], [FourthX+Grad[3]+7, 0]),
//     //             arc(6, [90, 270], [-(Grad[0]+7), 0]),
//     //         ]),
//     //         circle(axleEndRad+pivotClearence, [FourthX, 0]),
//     //         circle(axleEndRad+pivotClearence, [ThirdX, 0]),
//     //         circle(axleEndRad+pivotClearence, [centerX, 0]),
//     //         circle(axleEndRad+pivotClearence, [0, 0]),

//     //     ])]);
//     // }

//     // module holderOutline () {
//     //     finalize([difference() ([
//     //         shape([
//     //             arc(5, [-90, 90], [FourthX+Grad[3]+7, 0]),
//     //             arc(5, [90, 270], [ThirdX-(Grad[2]+7), 0]),
//     //         ]),
//     //         circle(axleEndRad+pivotClearence, [FourthX, 0]),
//     //         circle(axleEndRad+pivotClearence, [ThirdX, 0])

//     //     ])]);
//     // }

//     //  module holderOutline () {
//     //     finalize([difference() ([
//     //         shape([
//     //             arc(5, [0, 180], [FourthX, Grad[3]+7]),
//     //             arc(5, [180, 360], [FourthX, -(Grad[3]+7)]),
//     //         ]),
//     //         circle(axleEndRad+pivotClearence, [FourthX, 0]),
//     //         circle(axleEndRad+pivotClearence, [ThirdX, 0])

//     //     ])]);
//     // }

//     // // Top 
//     // union() {
//     //     translate([0,0,axleLen/2 - axleEndLen + bearingClearence/2])
//     //     linear_extrude(holderWidth)
//     //     holderOutline();
//     //     translate([FourthX, Grad[3]+7 ,-axleLen/2 + axleEndLen - holderWidth/2])
//     //     linear_extrude(axleLen - axleEndLen*2 + holderWidth)
//     //     circle(3.95);
//     //     translate([FourthX, -(Grad[2]+7),-axleLen/2 + axleEndLen - holderWidth/2])
//     //     linear_extrude(axleLen - axleEndLen*2 + holderWidth)
//     //     circle(3.95);
//     // }

//     // // Bottom
//     // difference() {
//     //     translate([0,0,-(axleLen/2 - axleEndLen + holderWidth + bearingClearence/2)])
//     //     linear_extrude(holderWidth)
//     //     holderOutline();
//     //     translate([FourthX, Grad[3]+7 ,-axleLen/2 + axleEndLen - holderWidth/2])
//     //     linear_extrude(axleLen - axleEndLen*2 + holderWidth)
//     //     circle(3.95);
//     //     translate([FourthX, -(Grad[2]+7),-axleLen/2 + axleEndLen - holderWidth/2])
//     //     linear_extrude(axleLen - axleEndLen*2 + holderWidth)
//     //     circle(3.95);
//     // }


//     // Top 
//     !union() {
//         translate([0,0,axleLen/2 - axleEndLen + bearingClearence/2])
//         linear_extrude(holderWidth)
//         holderOutline();
//         translate([FourthX + Grad[3]+7,0 ,-axleLen/2 + axleEndLen - holderWidth/2])
//         linear_extrude(axleLen - axleEndLen*2 + holderWidth)
//         circle(3.90);
//         translate([-(Grad[0]+7), 0,-axleLen/2 + axleEndLen - holderWidth/2])
//         linear_extrude(axleLen - axleEndLen*2 + holderWidth)
//         circle(3.90);
//     }

//     // Bottom
//     difference() {
//         translate([0,0,-(axleLen/2 - axleEndLen + holderWidth + bearingClearence/2)])
//         linear_extrude(holderWidth)
//         holderOutline();
//         translate([FourthX + Grad[3]+7,0 ,-axleLen/2 + axleEndLen - holderWidth/2])
//         linear_extrude(axleLen - axleEndLen*2 + holderWidth)
//         circle(4);
//         // translate([(Grad[0]+7),0 ,-axleLen/2 + axleEndLen - holderWidth/2])
//         // linear_extrude(axleLen - axleEndLen*2 + holderWidth)
//         // circle(4);
//         translate([-(Grad[0]+7), 0,-axleLen/2 + axleEndLen - holderWidth/2])
//         linear_extrude(axleLen - axleEndLen*2 + holderWidth)
//         circle(4);
//         translate([0,0,-50])
//         cube(200);
//     }
    

// }

// GearTrain(mods, gearZ, pinionZ, gearWidth, gearClearence, vertClearance);

// GearAndPinion(0.5, 1, gearWidth, vertClearance, 80, 12);
// translate([0, 0, -holderWidth-2.5])
//     GearShaft(holderWidth*2+gearWidth*2+vertClearance+5,
//             axleR, axleEndR, holderWidth, gearWidth*2+vertClearance);



// // Base for holding gears
// function gear_holderBase(r1, r2, AxleRad, gearClear) =
//     difference() ([
//         shape([

//             // Leftern arm holding gear 1
//             line([-5, 5], [-r1-gearClear/2, 5]),
//             arc(5, [90, 270], [-r1-gearClear/2, 0]),
//             line([-r1, -5], [-5, -5]),

//             // Lower arm
//             arc(5, [180, 360], [0, -15]),

//             // Right arm holding gear 1
//             line([5, -5], [r2+gearClear/2, -5]),
//             arc(5, [-90, 90], [r2+gearClear/2, 0]),
//             line([r2, 5], [5, 5]),

//             // Upper arm
//             arc(5, [0,180], [0,15]),
            
//         ]),

//         // Leftern axle hole
//         circle(AxleRad+0.2, [-r1-gearClear/2, 0]),
//         //Right axle hole
//         circle(AxleRad+0.2, [r2+gearClear/2, 0]),

//     ]);

// module GearShaft(len, rad, endRad, endLen, midLen) {
//     linear_extrude(len) circle(endRad);
    
//     translate([0,0,endLen])
//     linear_extrude((len-midLen)/2 - endLen, scale=rad/endRad)
//     circle(endRad);

//     translate([0,0,(len-midLen)/2])
//     linear_extrude(midLen)
//     circle(rad);

//     translate([0,0,len-(len-midLen)/2])
//     linear_extrude((len-midLen)/2 - endLen, scale=endRad/rad)
//     circle(rad);
// }

// // Bottom holder
// union() {
//     translate([0,0,-holderWidth])
//     linear_extrude(holderWidth)
//     finalize([
//         gear_holderBase(gear1Rad, gear2Rad, axleEndR, gearClearence)
//     ]);
//     linear_extrude(axleLength-holderWidth-(holderWidth/2)) {
//     finalize(circle(3.925, [0,15]));
//     finalize(circle(3.925, [0,-15]));
//     }
// }

// // Top holder
// difference() {
//     translate([0,0,axleLength-holderWidth*2])
//     linear_extrude(holderWidth)
//     finalize([
//         gear_holderBase(gear1Rad, gear2Rad, axleEndR, gearClearence)
//     ]);
//     linear_extrude(axleLength-holderWidth-(holderWidth/2)) {
//     finalize(circle(4, [0,15]));
//     finalize(circle(4, [0,-15]));
//     }

// }

// translate([-gear1Rad-gearClearence/2, 0, 0])
// union() {
//     translate([0, 0, -holderWidth])
//     GearShaft(axleLength, axleR, axleEndR, holderWidth, gearWidth);

//     translate([0, 0, (axleLength-gearWidth)/2 - holderWidth])
//     linear_extrude(gearWidth)
//     rotate ([0, 0, $t * 360/z1]) // $t * 360/gear1[1]
//         finalize(involuteGear_outline(mSize, z1, pitch));


// }

// !translate([gear2Rad+gearClearence/2, 0, 0])
// union() {
//     translate([0, 0, -holderWidth])
//     GearShaft(axleLength, axleR, axleEndR, holderWidth, gearWidth);

//     translate([0, 0, (axleLength-gearWidth)/2 - holderWidth])
//     linear_extrude(gearWidth)
//     rotate ([0, 0, $t * 360/z2 + (z2%2 - 1) * 180/z2]) // $t * 360/gear1[1]
//         finalize(involuteGear_outline(mSize, z2, pitch));
// }