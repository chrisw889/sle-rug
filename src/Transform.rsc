module Transform

import Syntax;
import Resolve;
import AST;
import ParseTree;

import IO;

/* 
 * Transforming QL forms
 */
 
 
/* Normalization:
 * 
 *  flatten function on AForm ast:
 * 
 *  wrt to the semantics of QL the following
 *     q0: "" int; 
 
 *     if (a) { 
 *        if (b) { 
 *          q1: "" int; 
 *        } 
 *        q2: "" int; 
 *      }
 *
 *  is converted to
 *     if (true) q0: "" int;
 *     if (true && a && b) q1: "" int;
 *     if (true && a) q2: "" int;
 *
 */

AForm flatten(AForm f) {
	return form(f.name, ( [] | it + flatten(a, boolean(true)) | AQuestion a <- f.questions ), src=f.src);
}

list[AQuestion] flatten(AQ(str q, AId d, AType dt), AExpr guard) 
	= [AQIf(guard, [AQ(q, d, dt)])];
list[AQuestion] flatten(AQAssign(str q, AId d, AType dt, AExpr e), AExpr guard)
	= [AQIf(guard, [AQAssign(q, d, dt, e)])];
list[AQuestion] flatten(AQIf(AExpr g, list[AQuestion] ify), AExpr guard)
	= ( [] | it + flatten(a, and(g, guard)) | AQuestion a <- ify );
list[AQuestion] flatten(AQIfElse(AExpr g, list[AQuestion] ify, list[AQuestion] ifn), AExpr guard)
	= ( [] | it + flatten(a, and(g, guard)) | AQuestion a <- ify )
	+ ( [] | it + flatten(a, and(not(g), guard)) | AQuestion a <- ify );

/* Rename refactoring:
 *
 * rename function on Form cst:
 * 
 * refactoring transformation that consistently renames all occurrences of the same name.
 * Uses the results of name resolution to find the equivalence class of a name.
 *
 */
 
start[Form] rename(start[Form] f, loc useOrDef, str newName, UseDef useDef) {
	set[loc] toRename = {useOrDef}
					  + { d | <useOrDef, loc d> <- useDef }
					  + { u | <loc u, useOrDef> <- useDef };

	return visit (f){
		case (Question)`<Str s><Id x>:<Type t>`
			=> (Question)`<Str s><Id nn>:<Type t>`
				when x@\loc in toRename, Id nn := [Id]newName
		case (Question)`<Str s><Id x>:<Type t>=<Expr e>`
			=> (Question)`<Str s><Id nn>:<Type t>=<Expr e>`
					when x@\loc in toRename, Id nn := [Id]newName
		case (Expr)`<Id x>`
			=> (Expr)`<Id nn>`
				when x@\loc in toRename, Id nn := [Id]newName
	}
}
