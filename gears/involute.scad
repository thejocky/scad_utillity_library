include <involute_outline.scad>
include <std_functions.scad>



/* 
 * saves specifications as lists through function alternative
 * used in saving gear specifications used as parameters in other modules
 *   involuteGear  : ["involuteGear",  width, m,    z,      p]
 *   gearShaft     : ["gearShaft",     len,   rad,  endRad, endLen, midLen]
 *   gearAndPinion : ["gearAndPinion", gear,  pinion]
 * 
 * 
 */

function involuteGear(width, m, z, p=20, spoked=undef, center=false) =
    ["involuteGear", width, m, z, p, spoked, center];

module involuteGear(width, m, z, p=20, spoked=undef, center=false) {
    if (is_list(width) && width[0] == "involuteGear") {
        involuteGear(width[1], width[2], width[3], width[4], width[5], width[6]);
    } else {
        translate([0,0,(center)?-width/2:0])
        linear_extrude(width)
        finalize(involuteGear_outline(m, z, p, spoked));
    }
}


function gearAndPinion(gear, pinion) =
    ["gearAndPinion", gear, pinion];

module gearAndPinion(gear, pinion) {
    if (is_list(gear) && gear[0] == "gearAndPinion") {
        involuteGear(gear[1], gear[2]);
    } else {
        // Create gear
        involuteGear(gear);
        // Create pinion with the width being the sum of pinion and gear so pinio rests ontop of gear
        involuteGear(pinion[1]+gear[1], pinion[2], pinion[3], pinion[4], pinion[5], pinion[6]);
    }
}


