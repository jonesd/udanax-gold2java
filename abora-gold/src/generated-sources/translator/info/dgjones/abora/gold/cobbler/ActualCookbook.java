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
import info.dgjones.abora.gold.collection.basic.Int32Array;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Recipe;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * We internally map from Category to preorder number for the category and lookup using that
 * preorder number.
 */
public class ActualCookbook extends Cookbook {

	protected String myName;
	protected Category myBootCategory;
	protected Cookbook myNext;
	protected PtrArray myRecipes;
	protected PtrArray myDecoding;
	protected Int32Array myEncoding;
	protected static Cookbook TheCookbooks;
/*
udanax-top.st:14137:
Cookbook subclass: #ActualCookbook
	instanceVariableNames: '
		myName {char star}
		myBootCategory {Category}
		myNext {Cookbook}
		myRecipes {PtrArray of: Recipe}
		myDecoding {PtrArray of: Category}
		myEncoding {UInt32Array}'
	classVariableNames: 'TheCookbooks {Cookbook} '
	poolDictionaries: ''
	category: 'Xanadu-cobbler'!
*/
/*
udanax-top.st:14147:
ActualCookbook comment:
'We internally map from Category to preorder number for the category and lookup using that preorder number.'!
*/
/*
udanax-top.st:14149:
(ActualCookbook getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:14240:
ActualCookbook class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:14243:
(ActualCookbook getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ActualCookbook.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Category bootCategory() {
	return myBootCategory;
/*
udanax-top.st:14154:ActualCookbook methodsFor: 'accessing'!
{Category} bootCategory
	^myBootCategory!
*/
}
public Recipe fetchRecipe(Category cat) {
	return (Recipe) (myRecipes.fetch(cat.preorderNumber()));
/*
udanax-top.st:14157:ActualCookbook methodsFor: 'accessing'!
{Recipe} fetchRecipe: cat {Category}
	^(myRecipes fetch: cat preorderNumber) cast: Recipe!
*/
}
public Category getCategoryFor(int no) {
	Category category;
	category = (Category) (myDecoding.fetch(no));
	if (category == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
	return category;
/*
udanax-top.st:14160:ActualCookbook methodsFor: 'accessing'!
{Category} getCategoryFor: no {IntegerVar}
	| category {Category} |
	category _ (myDecoding fetch: no DOTasLong) cast: Category.
	category == NULL ifTrue: [Heaper BLAST: #NotInTable].
	^category!
*/
}
public Recipe getRecipe(Category cat) {
	Recipe recipe;
	recipe = (Recipe) (myRecipes.fetch(cat.preorderNumber()));
	if (recipe == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
	return recipe;
/*
udanax-top.st:14166:ActualCookbook methodsFor: 'accessing'!
{Recipe} getRecipe: cat {Category}
	| recipe {Recipe} |
	recipe _ (myRecipes fetch: cat preorderNumber) cast: Recipe.
	recipe == NULL ifTrue: [Heaper BLAST: #NotInTable].
	^recipe!
*/
}
public String id() {
	return myName;
/*
udanax-top.st:14172:ActualCookbook methodsFor: 'accessing'!
{char star} id
	^myName!
*/
}
public Cookbook next() {
	return myNext;
/*
udanax-top.st:14175:ActualCookbook methodsFor: 'accessing'!
{Cookbook} next
	^myNext!
*/
}
public int numberOfCategory(Category cat) {
	int num;
	num = myEncoding.uIntAt(cat.preorderNumber());
	if (num >= myRecipes.count()) {
		throw new AboraRuntimeException(AboraRuntimeException.UNENCODED_CATEGORY);
	}
	return num;
/*
udanax-top.st:14178:ActualCookbook methodsFor: 'accessing'!
{IntegerVar} numberOfCategory: cat {Category}
	| num {Int32} |
	num _ myEncoding uIntAt: cat preorderNumber.
	num >= myRecipes count ifTrue: [Heaper BLAST: #UnencodedCategory].
	^num!
*/
}
public PtrArray recipes() {
	return myRecipes;
/*
udanax-top.st:14184:ActualCookbook methodsFor: 'accessing'!
{PtrArray} recipes 
	^myRecipes!
*/
}
public ActualCookbook(Category cat, String id, PtrArray recipes, int count) {
	super();
	int preorderLimit;
	int code;
	myName = id;
	myBootCategory = cat;
	preorderLimit = Heaper.preorderMax() + 1;
	/* preorder -> recipe. */
	myRecipes = recipes;
	/* preorder -> code. */
	myEncoding = Int32Array.make(preorderLimit);
	/* code -> category */
	myDecoding = PtrArray.nulls(count);
	code = 0;
	for (int i = 0; i < preorderLimit; i ++ ) {
		Recipe recipe;
		recipe = (Recipe) (myRecipes.fetch(i));
		if (recipe == null) {
			myEncoding.storeUInt(i, preorderLimit);
		}
		else {
			myEncoding.storeUInt(i, code);
			myDecoding.store(code, recipe.categoryOfDish());
			code = code + 1;
		}
	}
	myNext = TheCookbooks;
	TheCookbooks = this;
/*
udanax-top.st:14189:ActualCookbook methodsFor: 'creation'!
create: cat {Category} with: id {char star} with: recipes {PtrArray of: Recipe} with: count {Int32}
	| preorderLimit {Int32} code {Int32} |
	super create.
	myName _ id.
	myBootCategory _ cat.
	preorderLimit _ Heaper preorderMax + 1.
	"preorder -> recipe."
	myRecipes _ recipes.
	"preorder -> code."
	myEncoding _ UInt32Array make: preorderLimit.
	"code -> category"
	myDecoding _ PtrArray nulls: count.
	code _ Int32Zero.
	Int32Zero almostTo: preorderLimit do: 
		[:i {Int32} |
		| recipe {Recipe} |
		recipe _ (myRecipes fetch: i) cast: Recipe.
		recipe == NULL
			ifTrue: [myEncoding at: i storeUInt: preorderLimit]
			ifFalse:
				[myEncoding at: i storeUInt: code.
				myDecoding at: code store: recipe categoryOfDish.
				code _ code + 1]].
	myNext _ TheCookbooks.
	TheCookbooks _ self!
*/
}
/**
 * ActualCookbooks last for the whole run.
 */
public void destroy() {
/*
udanax-top.st:14215:ActualCookbook methodsFor: 'creation'!
{void} destroy
	"ActualCookbooks last for the whole run."!
*/
}
public void printOn(PrintWriter oo) {
	oo.print("an ");
	oo.print(getAboraClass().name());
/*
udanax-top.st:14220:ActualCookbook methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << 'an ' << self getCategory name!
*/
}
public void receiveClassList(Rcvr rcvr) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
udanax-top.st:14225:ActualCookbook methodsFor: 'smalltalk: hooks:'!
{void RECEIVE.HOOK} receiveClassList: rcvr {Rcvr}
	| count {IntegerVar} |
	count _ rcvr receiveIntegerVar.
	myRecipes  _ MuTable make: HeaperSpace make.
	Int32Zero almostTo: count do: 
		[:i {Int32} | | clName {String} cl {Category} |
		clName _ rcvr receiveString.
		[cl _ Smalltalk at: clName asSymbol ifAbsent: [Cookbook BLAST: 'class name not recognized']] smalltalkOnly.
		myRecipes at: cl store: cl getRecipe.]!
*/
}
public void sendClassList(Xmtr xmtr) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
udanax-top.st:14235:ActualCookbook methodsFor: 'smalltalk: hooks:'!
{void SEND.HOOK} sendClassList: xmtr {Xmtr}
	xmtr sendIntegerVar: myRecipes count.
	myRecipes stepper forEach: [:rec | xmtr sendString: rec categoryOfDish name]!
*/
}
public static int addCuisineTo(Recipe cuisine, PtrArray recipes) {
	Recipe recipe;
	int count;
	count = 0;
	recipe = cuisine;
	while (recipe != null) {
		recipes.store(recipe.categoryOfDish().preorderNumber(), recipe);
		count = count + 1;
		recipe = recipe.next();
	}
	return count;
/*
udanax-top.st:14248:ActualCookbook class methodsFor: 'global: utility'!
{Int32} addCuisine: cuisine {Recipe} to: recipes {PtrArray}
	| recipe {Recipe} count {Int32} |
	count _ Int32Zero.
	recipe _ cuisine.
	[recipe ~~ NULL] whileTrue:
		[recipes at: recipe categoryOfDish preorderNumber store: recipe.
		count _ count + 1.
		recipe _ recipe next].
	^count!
*/
}
public static Cookbook makeCategory(Category bootCat) {
	Cookbook cookbook;
	cookbook = TheCookbooks;
	while (cookbook != null) {
		if (cookbook.bootCategory().isEqual(bootCat)) {
			return cookbook;
		}
		cookbook = cookbook.next();
	}
	throw new AboraRuntimeException(AboraRuntimeException.UNKNOWN_COOKBOOK);
/*
udanax-top.st:14260:ActualCookbook class methodsFor: 'creation'!
{Cookbook} make.Category: bootCat {Category}
	| cookbook {Cookbook} |
	cookbook _ TheCookbooks.
	[cookbook ~~ NULL] whileTrue:
		[(cookbook bootCategory isEqual: bootCat) ifTrue: [^cookbook].
		cookbook _ cookbook next].
	Heaper BLAST: #UnknownCookbook.
	^NULL "fodder"!
*/
}
public static Cookbook makeString(String id) {
	Cookbook cookbook;
	cookbook = TheCookbooks;
	while (cookbook != null) {
		if ((cookbook.id().compareTo(id)) == 0) {
			return cookbook;
		}
		cookbook = cookbook.next();
	}
	throw new AboraRuntimeException(AboraRuntimeException.UNKNOWN_COOKBOOK);
/*
udanax-top.st:14269:ActualCookbook class methodsFor: 'creation'!
{Cookbook} make.String: id {char star}
	| cookbook {Cookbook} |
	cookbook _ TheCookbooks.
	[cookbook ~~ NULL] whileTrue:
		[(String strcmp: cookbook id with: id) == Int32Zero ifTrue: [^cookbook].
		cookbook _ cookbook next].
	Heaper BLAST: #UnknownCookbook.
	^NULL "fodder"!
*/
}
public static void cleanupGarbage() {
	TheCookbooks = null;
/*
udanax-top.st:14280:ActualCookbook class methodsFor: 'smalltalk: initialization'!
{void} cleanupGarbage
	TheCookbooks _ NULL!
*/
}
public static void initTimeNonInherited() {
	Cookbook.declareCookbook("empty", AboraSupport.findCategory(Heaper.class), null);
/*
udanax-top.st:14283:ActualCookbook class methodsFor: 'smalltalk: initialization'!
initTimeNonInherited
	Cookbook declareCookbook: 'empty' with: Heaper with: NULL!
*/
}
public static void linkTimeNonInherited() {
	TheCookbooks = null;
/*
udanax-top.st:14286:ActualCookbook class methodsFor: 'smalltalk: initialization'!
{void} linkTimeNonInherited
	TheCookbooks _ NULL!
*/
}
public ActualCookbook() {
/*

Generated during transformation
*/
}
public ActualCookbook(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
