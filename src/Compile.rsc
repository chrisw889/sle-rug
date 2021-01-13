module Compile

import AST;
import Resolve;
import IO;
import lang::html5::DOM; // see standard library

/*
 * Implement a compiler for QL to HTML and Javascript
 *
 * - assume the form is type- and name-correct
 * - separate the compiler in two parts form2html and form2js producing 2 files
 * - use string templates to generate Javascript
 * - use the HTML5Node type and the `str toString(HTML5Node x)` function to format to string
 * - use any client web framework (e.g. Vue, React, jQuery, whatever) you like for event handling
 * - map booleans to checkboxes, strings to textfields, ints to numeric text fields
 * - be sure to generate uneditable widgets for computed questions!
 * - if needed, use the name analysis to link uses to definitions
 */

void compile(AForm f) {
  print(f.src);
  writeFile(f.src[extension="js"].top, form2js(f));
  writeFile(f.src[extension="html"].top, toString(form2html(f)));
}

HTML5Node form2html(AForm f)
  = html(
  	head(
  		title("<f.name>"),
  		style("form {
  	    '	max-width: 3000px;
  		'	display: block;
  		'	margin: 0 auto;
  		'}")
  	),
  	body(
  		h1("<f.name>"),
  		form(fieldDefs(f)),
  		style(src("jquery-3.5.1.min.js")),
  		style(src("<f.src[extension="js"].path>.js"))
  	)
  );
  
list[HTML5Node] fieldDefs(AForm f) 
	= ([] | it + fieldDefs(a) | AQuestion a <- f.questions);
list[HTML5Node] fieldDefs(AQ(str q, id(str n), AType dt)) 
	= [div(
		label(\for("<n>"), "<q>"),
		input(\type(TInpType(dt)), \value(TDefault(dt)), id("<n>"))
	)];
list[HTML5Node] fieldDefs(AQAssign(str q, id(str n), AType dt, _)) 
	= [div(
		label(\for("<n>"), "<q>"),
		input(\type(TInpType(dt)), \value(TDefault(dt)), id("<n>"), readonly(""))
	)];
list[HTML5Node] fieldDefs(AQIf(_, list[AQuestion] IfYes)) 
	= ([] | it + fieldDefs(a) | AQuestion a <- IfYes);
list[HTML5Node] fieldDefs(AQIfElse(_, list[AQuestion] IfYes, list[AQuestion] IfNo)) 
	= ([] | it + fieldDefs(a) | AQuestion a <- ifYes)
	+ ([] | it + fieldDefs(a) | AQuestion a <- ifNo);

str TDefault(integer()) = "0";
str TDefault(boolean()) = "false";
str TDefault(string()) = "";

str TInpType(integer()) = "number";
str TInpType(boolean()) = "checkbox";
str TInpType(string()) = "text";

str form2js(AForm f)
	= "<for (AQuestion a <- f.questions){>
 	'<varDefs(a)><}>
  	'<for (AQuestion a <- f.questions) {>
  	'<qUpdates(a)><}>";

str varDefs(AQ(_, id(str n), _)) = "let <n> = $(\"#<n>\");";
str varDefs(AQAssign(_, id(str n), _, _)) = "let <n> = $(\"#<n>\");";
str varDefs(AQIf(_, list[AQuestion] ifYes)) 
  = "<for (AQuestion a <- ifYes) {>
    '<varDefs(a)><}>";
str varDefs(AQIfElse(_, list[AQuestion] ifYes, list[AQuestion] ifNo)) 
  = "<for (AQuestion a <- ifYes) {>
    '<varDefs(a)><}><for (AQuestion a <- ifNo) {>
    '<varDefs(a)><}>";
  
str qUpdates(AQuestion a) = "";