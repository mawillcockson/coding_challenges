//
// Loop bodies are blocks, which are also expressions. We've seen
// how they can be used to evaluate and return values. To further
// expand on this concept, it turns out we can also give names to
// blocks by applying a 'label':
//
//     my_label: { ... }
//
// Once you give a block a label, you can use 'break' to exit
// from that block.
//
//     outer_block: {           // outer block
//         while (true) {       // inner block
//             break :outer_block;
//         }
//         unreachable;
//     }
//
// As we've just learned, you can return a value using a break
// statement. Does that mean you can return a value from any
// labeled block? Yes it does!
//
//     const foo = make_five: {
//         const five = 1 + 1 + 1 + 1 + 1;
//         break :make_five five;
//     };
//
// Labels can also be used with loops. Being able to break out of
// nested loops at a specific level is one of those things that
// you won't use every day, but when the time comes, it's
// incredibly convenient. Being able to return a value from an
// inner loop is sometimes so handy, it almost feels like cheating
// (and can help you avoid creating a lot of temporary variables).
//
//     const bar: u8 = two_loop: while (true) {
//         while (true) {
//             break :two_loop 2;
//         }
//     } else 0;
//
// In the above example, the break exits from the outer loop
// labeled "two_loop" and returns the value 2. The else clause is
// attached to the outer two_loop and would be evaluated if the
// loop somehow ended without the break having been called.
//
// Finally, you can also use block labels with the 'continue'
// statement:
//
//     my_while: while (true) {
//         continue :my_while;
//     }
//
const std = @import("std");
const print = std.debug.print;

const Ingredient = enum {
    Chili,
    Macaroni,
    Tomato_Sauce,
    Cheese,
};

const IngredientSet = std.enums.EnumSet(Ingredient);

const Food = struct {
    name: []const u8,
    requires: IngredientSet,
};

//                 Chili  Macaroni  Tomato Sauce  Cheese
// ------------------------------------------------------
//  Mac & Cheese              x                     x
//  Chili Mac        x        x
//  Pasta                     x          x
//  Cheesy Chili     x                              x
// ------------------------------------------------------

const menu = [_]Food{
    Food{
        .name = "Mac & Cheese",
        .requires = IngredientSet.initMany(&[_]Ingredient{ .Macaroni, .Cheese }),
    },
    Food{
        .name = "Chili Mac",
        .requires = IngredientSet.initMany(&[_]Ingredient{ .Chili, .Macaroni }),
    },
    Food{
        .name = "Pasta",
        .requires = IngredientSet.initMany(&[_]Ingredient{ .Macaroni, .Tomato_Sauce }),
    },
    Food{
        .name = "Cheesy Chili",
        .requires = IngredientSet.initMany(&[_]Ingredient{ .Chili, .Cheese }),
    },
};

pub fn main() void {
    // Welcome to Cafeteria USA! Choose your favorite ingredients
    // and we'll produce a delicious meal.
    //
    // Cafeteria Customer Note: Not all ingredient combinations
    // make a meal. The default meal is macaroni and cheese.
    //
    // Software Developer Note: Hard-coding the ingredient
    // numbers (based on array position) will be fine for our
    // tiny example, but it would be downright criminal in a real
    // application!
    const wanted_ingredients = IngredientSet.initMany(&[_]Ingredient{
        .Chili,
        .Cheese,
    });

    const meal: Food = food_loop: for (menu) |food| {
        // Look at each Food on the menu and if the food contains the wanted
        // ingredients, return it
        if (food.requires.supersetOf(wanted_ingredients)) {
            break :food_loop food;
        }
    } else menu[0];
    // Return Mac & Cheese as the default Food when the requested ingredients
    // aren't found.

    print("Enjoy your {s}!\n", .{meal.name});
}

// Challenge: You can also do away with the 'found' variable in
// the inner loop. See if you can figure out how to do that!
