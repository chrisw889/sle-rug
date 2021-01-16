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
  print("compiled");
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
  		script(src("jquery-3.5.1.min.js")),
  		script(src("<f.src[extension="js"].file>"))
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
		input(\type(TInpType(dt)), \value((dt := boolean()) ? "unchecked" : TDefault(dt)), id("<n>"), readonly(""))
	)];
list[HTML5Node] fieldDefs(AQIf(_, list[AQuestion] ify)) 
	= ([] | it + fieldDefs(a) | AQuestion a <- ify);
list[HTML5Node] fieldDefs(AQIfElse(_, list[AQuestion] ify, list[AQuestion] ifn)) 
	= ([] | it + fieldDefs(a) | AQuestion a <- ify)
	+ ([] | it + fieldDefs(a) | AQuestion a <- ifn);

str TDefault(integer()) = "0";
str TDefault(boolean()) = "false";
str TDefault(string()) = "\"\"";

str TInpType(integer()) = "number";
str TInpType(boolean()) = "checkbox";
str TInpType(string()) = "text";

str form2js(AForm f)
	= "<for (AQuestion a <- f.questions){>
 	'<varDefs(a)><}>
  	'<for (AQuestion a <- f.questions) {>
  	'<changeCalls(a)><}>
  	'
  	'function formUpdates(){<for (AQuestion a <- f.questions){>
  	'<qUpdates(a)><}>
  	'}
  	'formUpdates();";

str varDefs(AQ(_, id(str n), AType dt)) 
  = "let <n> = $(\"#<n>\");
    'let <n>_val = <TDefault(dt)>;";
str varDefs(AQAssign(_, id(str n), AType dt, _)) 
  = "let <n> = $(\"#<n>\");
    'let <n>_val = <TDefault(dt)>;";
str varDefs(AQIf(_, list[AQuestion] ify)) 
  = "<for (AQuestion a <- ify) {>
    '<varDefs(a)><}>";
str varDefs(AQIfElse(_, list[AQuestion] ify, list[AQuestion] ifn)) 
  = "<for (AQuestion a <- ify) {>
    '<varDefs(a)><}><for (AQuestion a <- ifn) {>
    '<varDefs(a)><}>";
  
str changeCalls(AQ(_, id(str n), _)) 
  = "<n>.change(function() {
    '  formUpdates();
    '});";
str changeCalls(AQAssign(_, id(str n), _, _))
  = "<n>.change(function() {
  	'  formUpdates();
  	'});";
str changeCalls(AQIf(_, list[AQuestion] ify))
  = "<for (AQuestion a <- ify) {>
    '<changeCalls(a)><}>";
str changeCalls(AQIfElse(_, list[AQuestion] ify, list[AQuestion] ifn))
  = "<for (AQuestion a <- ify) {>
    '<changeCalls(a)><}><for (AQuestion a <- ifn) {>
    '<changeCalls(a)><}>";
    
    //formUpdates
str qUpdates(AQ(_, id(str n), AType dt)) 
  = (boolean() := dt) ? "<n>_val = <n>.is(\':checked\');" : "<n>_val = <n>.val();" ;
str qUpdates(AQAssign(_, id(str n), AType dt, AExpr e)) 
  = (boolean() := dt) ? "<n>.prop(\'checked\', <aExpr2js(e)>);
    'if (<n>.is(\':checked\') != <n>_val){
    '	<n>_val = <n>.is(\':checked\');
    '}" : "<n>.val(<aExpr2js(e)>);
  	'if (<n>.val != <n>_val){
    '	<n>_val = <n>.val();
    '}";
str qUpdates(AQIf(AExpr g, list[AQuestion] ify))
  = "if (<aExpr2js(g)>) {
  	'	<for (AQuestion a <- ify) {><qShow(a)><}>
  	'	<for (AQuestion a <- ify) {>
  	'	<qUpdates(a)>
  	'	<}>
  	'} else {
  	'	<for (AQuestion a <- ify) {><qHide(a)><}>
  	'}";
str qUpdates(AQIfElse(AExpr g, list[AQuestion] ify, list[AQuestion] ifn))
  = "if (<aExpr2js(g)>) {
  	'	<for (AQuestion a <- ify) {><qShow(a)><}>
  	'	<for (AQuestion a <- ifn) {><qHide(a)><}>
  	'	<for (AQuestion a <- ify) {>
  	'	<qUpdates(a)>
  	'	<}>
  	'} else {
  	'	<for (AQuestion a <- ifn) {><qShow(a)><}>
  	'	<for (AQuestion a <- ify) {><qHide(a)><}>
  	'	<for (AQuestion a <- ifn) {>
  	'	<qUpdates(a)>
  	'	<}>
  	'}";
  
str qShow(AQ(_, id(str n), _)) = "<n>.show();\n\t$(\'label[for=<n>]\').show();\n";
str qShow(AQAssign(_, id(str n), _, _)) = "<n>.show();\n\t$(\'label[for=<n>]\').show();\n";
default str qShow(AQuestion _) = "";

str qHide(AQ(_, id(str n), _)) = "<n>.hide();\n\t$(\'label[for=<n>]\').hide();\n";
str qHide(AQAssign(_, id(str n), _, _)) = "<n>.hide();\n\t$(\'label[for=<n>]\').hide();\n";
default str qHide(AQuestion _) = "";

str aExpr2js(ref(id(str n))) = "<n>_val";
str aExpr2js(boolean(bool b)) = "<b>";
str aExpr2js(number(int n)) = "<n>";
str aExpr2js(not(AExpr e)) = "!<aExpr2js(e)>";
str aExpr2js(mult(AExpr l, AExpr r)) = "(<aExpr2js(l)>*<aExpr2js(r)>)";
str aExpr2js(div(AExpr l, AExpr r)) = "Math.floor(<aExpr2js(l)>/<aExpr2js(r)>)";
str aExpr2js(add(AExpr l, AExpr r)) = "(<aExpr2js(l)>+<aExpr2js(r)>)";
str aExpr2js(sub(AExpr l, AExpr r)) = "(<aExpr2js(l)>-<aExpr2js(r)>)";
str aExpr2js(gt(AExpr l, AExpr r)) = "(<aExpr2js(l)>\><aExpr2js(r)>)";
str aExpr2js(lt(AExpr l, AExpr r)) = "(<aExpr2js(l)>\<<aExpr2js(r)>)";
str aExpr2js(geq(AExpr l, AExpr r)) = "(<aExpr2js(l)>\>=<aExpr2js(r)>)";
str aExpr2js(leq(AExpr l, AExpr r)) = "(<aExpr2js(l)>\<=<aExpr2js(r)>)";
str aExpr2js(equ(AExpr l, AExpr r)) = "(<aExpr2js(l)>==<aExpr2js(r)>)";
str aExpr2js(neq(AExpr l, AExpr r)) = "(<aExpr2js(l)>!=<aExpr2js(r)>)";
str aExpr2js(and(AExpr l, AExpr r)) = "(<aExpr2js(l)>&&<aExpr2js(r)>)";
str aExpr2js(or(AExpr l, AExpr r)) = "(<aExpr2js(l)>||<aExpr2js(r)>)";