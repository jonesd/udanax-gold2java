/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.cobbler;

import info.dgjones.abora.gold.cobbler.ActualCookbook;
import info.dgjones.abora.gold.cobbler.Cookbook;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.basic.WeakPtrArray;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.wparray.XnExecutor;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Recipe;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class Cookbook extends Heaper {

/*
udanax-top.st:14022:
Heaper subclass: #Cookbook
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-cobbler'!
*/
/*
udanax-top.st:14026:
(Cookbook getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
/*
udanax-top.st:14071:
Cookbook class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:14074:
(Cookbook getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Cookbook.class).setAttributes( new Set().add("DEFERRED").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public Category bootCategory() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:14031:Cookbook methodsFor: 'accessing'!
{Category} bootCategory
	self subclassResponsibility!
*/
}
public Recipe fetchRecipe(Category cat) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:14034:Cookbook methodsFor: 'accessing'!
{Recipe} fetchRecipe: cat {Category}
	self subclassResponsibility!
*/
}
public Category getCategoryFor(int no) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:14037:Cookbook methodsFor: 'accessing'!
{Category} getCategoryFor: no {IntegerVar}
	self subclassResponsibility!
*/
}
public Recipe getRecipe(Category cat) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:14040:Cookbook methodsFor: 'accessing'!
{Recipe} getRecipe: cat {Category}
	self subclassResponsibility!
*/
}
/**
 * return a string that uniquely determines the version of the cookbook. It
 * should change whenever classes are added or removed, or when their storage
 * or transmission protocol changes
 */
public String id() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:14043:Cookbook methodsFor: 'accessing'!
{char star} id
	"return a string that uniquely determines the version of the cookbook. It 
	should change whenever classes are added or removed, or when their storage 
	or transmission protocol changes"
	self subclassResponsibility!
*/
}
public Cookbook next() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:14050:Cookbook methodsFor: 'accessing'!
{Cookbook} next
	self subclassResponsibility!
*/
}
public int numberOfCategory(Category cat) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:14053:Cookbook methodsFor: 'accessing'!
{IntegerVar} numberOfCategory: cat {Category}
	self subclassResponsibility!
*/
}
public PtrArray recipes() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:14056:Cookbook methodsFor: 'accessing'!
{PtrArray} recipes 
	self subclassResponsibility!
*/
}
public void printOn(PrintWriter oo) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:14061:Cookbook methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	self subclassResponsibility!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:14066:Cookbook methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:14068:Cookbook methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
/**
 * Create and register a cookbook.  The cookbook can be looked up according to etiher its
 * name or bootCategory.
 */
public static Cookbook declareCookbook(String id, Category bootCat, Recipe cuisine) {
	PtrArray recipes;
	int count;
	/* preorder -> recipe. */
	recipes = WeakPtrArray.make(XnExecutor.noopExecutor(), Heaper.preorderMax() + 1);
	count = ActualCookbook.addCuisineTo(cuisine, recipes);
	return new ActualCookbook(bootCat, id, recipes, count);
/*
udanax-top.st:14079:Cookbook class methodsFor: 'declaring'!
{Cookbook} declareCookbook: id {char star} with: bootCat {Category} with: cuisine {Recipe}
	"Create and register a cookbook.  The cookbook can be looked up according to etiher its name or bootCategory."
	
	| recipes {PtrArray} count {Int32} |
	"preorder -> recipe."
	recipes _ WeakPtrArray make: XnExecutor noopExecutor with: Heaper preorderMax + 1.
	count _ ActualCookbook addCuisine: cuisine to: recipes.
	^ActualCookbook create: bootCat with: id with: recipes with: count!
*/
}
/**
 * Create and register a cookbook.  The cookbook can be looked up according to etiher its
 * name or bootCategory.
 */
public static Cookbook declareCookbook(String id, Category bootCat, Recipe cuisine1, Recipe cuisine2) {
	PtrArray recipes;
	int count;
	/* preorder -> recipe. */
	recipes = WeakPtrArray.make(XnExecutor.noopExecutor(), Heaper.preorderMax() + 1);
	count = ActualCookbook.addCuisineTo(cuisine1, recipes);
	count = count + (ActualCookbook.addCuisineTo(cuisine2, recipes));
	return new ActualCookbook(bootCat, id, recipes, count);
/*
udanax-top.st:14088:Cookbook class methodsFor: 'declaring'!
{Cookbook} declareCookbook: id {char star} with: bootCat {Category} with: cuisine1 {Recipe} with: cuisine2 {Recipe}
	"Create and register a cookbook.  The cookbook can be looked up according to etiher its name or bootCategory."
	
	| recipes {PtrArray} count {Int32} |
	"preorder -> recipe."
	recipes _ WeakPtrArray make: XnExecutor noopExecutor with: Heaper preorderMax + 1.
	count _ ActualCookbook addCuisine: cuisine1 to: recipes.
	count _ count + (ActualCookbook addCuisine: cuisine2 to: recipes).
	^ActualCookbook create: bootCat with: id with: recipes with: count!
*/
}
/**
 * Create and register a cookbook.  The cookbook can be looked up according to etiher its
 * name or bootCategory.
 */
public static Cookbook declareCookbook(String id, Category bootCat, Recipe cuisine1, Recipe cuisine2, Recipe cuisine3) {
	PtrArray recipes;
	int count;
	/* preorder -> recipe. */
	recipes = WeakPtrArray.make(XnExecutor.noopExecutor(), Heaper.preorderMax() + 1);
	count = ActualCookbook.addCuisineTo(cuisine1, recipes);
	count = count + (ActualCookbook.addCuisineTo(cuisine2, recipes));
	count = count + (ActualCookbook.addCuisineTo(cuisine3, recipes));
	return new ActualCookbook(bootCat, id, recipes, count);
/*
udanax-top.st:14098:Cookbook class methodsFor: 'declaring'!
{Cookbook} declareCookbook: id {char star} with: bootCat {Category} with: cuisine1 {Recipe} with: cuisine2 {Recipe} with: cuisine3 {Recipe}
	"Create and register a cookbook.  The cookbook can be looked up according to etiher its name or bootCategory."
	
	| recipes {PtrArray} count {Int32} |
	"preorder -> recipe."
	recipes _ WeakPtrArray make: XnExecutor noopExecutor with: Heaper preorderMax + 1.
	count _ ActualCookbook addCuisine: cuisine1 to: recipes.
	count _ count + (ActualCookbook addCuisine: cuisine2 to: recipes).
	count _ count + (ActualCookbook addCuisine: cuisine3 to: recipes).
	^ActualCookbook create: bootCat with: id with: recipes with: count!
*/
}
/**
 * Create and register a cookbook.  The cookbook can be looked up according to etiher its
 * name or bootCategory.
 */
public static Cookbook declareCookbook(String id, Category bootCat, Recipe cuisine1, Recipe cuisine2, Recipe cuisine3, Recipe cuisine4) {
	PtrArray recipes;
	int count;
	/* preorder -> recipe. */
	recipes = WeakPtrArray.make(XnExecutor.noopExecutor(), Heaper.preorderMax() + 1);
	count = ActualCookbook.addCuisineTo(cuisine1, recipes);
	count = count + (ActualCookbook.addCuisineTo(cuisine2, recipes));
	count = count + (ActualCookbook.addCuisineTo(cuisine3, recipes));
	count = count + (ActualCookbook.addCuisineTo(cuisine4, recipes));
	return new ActualCookbook(bootCat, id, recipes, count);
/*
udanax-top.st:14109:Cookbook class methodsFor: 'declaring'!
{Cookbook} declareCookbook: id {char star} with: bootCat {Category} with: cuisine1 {Recipe} with: cuisine2 {Recipe} with: cuisine3 {Recipe} with: cuisine4 {Recipe}
	"Create and register a cookbook.  The cookbook can be looked up according to etiher its name or bootCategory."
	
	| recipes {PtrArray} count {Int32} |
	"preorder -> recipe."
	recipes _ WeakPtrArray make: XnExecutor noopExecutor with: Heaper preorderMax + 1.
	count _ ActualCookbook addCuisine: cuisine1 to: recipes.
	count _ count + (ActualCookbook addCuisine: cuisine2 to: recipes).
	count _ count + (ActualCookbook addCuisine: cuisine3 to: recipes).
	count _ count + (ActualCookbook addCuisine: cuisine4 to: recipes).
	^ActualCookbook create: bootCat with: id with: recipes with: count!
*/
}
/**
 * Just return the empty cookbook.
 */
public static Cookbook make() {
	return ActualCookbook.makeString("empty");
/*
udanax-top.st:14123:Cookbook class methodsFor: 'creation'!
{Cookbook} make
	"Just return the empty cookbook."
	^ActualCookbook make.String: 'empty'!
*/
}
/**
 * Return the cookbook registered for the given bootCategory.
 */
public static Cookbook makeCategory(Category bootCat) {
	return ActualCookbook.makeCategory(bootCat);
/*
udanax-top.st:14127:Cookbook class methodsFor: 'creation'!
{Cookbook} make.Category: bootCat {Category}
	"Return the cookbook registered for the given bootCategory."
	
	^ActualCookbook make.Category: bootCat!
*/
}
/**
 * Return the cookbook registered for the given string.
 */
public static Cookbook makeString(String id) {
	return ActualCookbook.makeString(id);
/*
udanax-top.st:14132:Cookbook class methodsFor: 'creation'!
{Cookbook} make.String: id {char star}
	"Return the cookbook registered for the given string."
	^ActualCookbook make.String: id!
*/
}
public Cookbook() {
/*

Generated during transformation
*/
}
public Cookbook(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
