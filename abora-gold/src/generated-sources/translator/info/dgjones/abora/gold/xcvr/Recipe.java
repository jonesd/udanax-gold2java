/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xcvr;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.ActualCopyRecipe;
import info.dgjones.abora.gold.java.missing.CxxClassDescription;
import info.dgjones.abora.gold.java.missing.PseudoCopyRecipe;
import info.dgjones.abora.gold.java.missing.smalltalk.AboraClass;
import info.dgjones.abora.gold.java.missing.smalltalk.Association;
import info.dgjones.abora.gold.java.missing.smalltalk.OrderedCollection;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.java.missing.smalltalk.Smalltalk;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Recipe;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * The table of all recipes in the system is maintained in the Cookbook module.
 * Subclasses know how to craete instances of a particular class.
 */
public class Recipe extends Heaper {

	protected Category myCat;
	protected Recipe myNext;
/*
udanax-top.st:41821:
Heaper subclass: #Recipe
	instanceVariableNames: '
		myCat {Category}
		myNext {Recipe}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xcvr'!
*/
/*
udanax-top.st:41827:
Recipe comment:
'The table of all recipes in the system is maintained in the Cookbook module.  
Subclasses know how to craete instances of a particular class.'!
*/
/*
udanax-top.st:41830:
(Recipe getOrMakeCxxClassDescription)
	friends:
'friend class Cookbook;
friend Int4  addCuisineTo (APTR(Recipe) cuisine, APTR(PtrArray) recipes);';
	attributes: ((Set new) add: #NO.GC; add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:41876:
Recipe class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:41879:
(Recipe getOrMakeCxxClassDescription)
	friends:
'friend class Cookbook;
friend Int4  addCuisineTo (APTR(Recipe) cuisine, APTR(PtrArray) recipes);';
	attributes: ((Set new) add: #NO.GC; add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Recipe.class).setAttributes( new Set().add("NOGC").add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public Category categoryOfDish() {
	return myCat;
/*
udanax-top.st:41838:Recipe methodsFor: 'accessing'!
{Category} categoryOfDish
	^myCat!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(myCat);
	oo.print(")");
/*
udanax-top.st:41844:Recipe methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(' << myCat << ')'.!
*/
}
public int actualHashForEqual() {
	return categoryOfDish().hashForEqual() * 701;
/*
udanax-top.st:41849:Recipe methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^self categoryOfDish hashForEqual * 701!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof Recipe) {
		Recipe rec = (Recipe) other;
		return rec.categoryOfDish().isEqual(categoryOfDish());
	}
	else {
		return false;
	}
/*
udanax-top.st:41852:Recipe methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	other cast: Recipe 
		into: [:rec | ^rec categoryOfDish isEqual: self categoryOfDish]
		others: [^false].
	^false!
*/
}
/**
 * Returnt the next recipe in the receiver's cuisine.
 */
public Recipe next() {
	return myNext;
/*
udanax-top.st:41860:Recipe methodsFor: 'private: accessing'!
{Recipe} next
	"Returnt the next recipe in the receiver's cuisine."
	
	^myNext!
*/
}
/**
 * cuisine points to the *variable* in which the receiver should be registered.
 */
public Recipe(Category cat, Association cuisine) {
	super();
	myCat = cat;
	myNext = cuisine.refValue();
	cuisine.refAssign(this);
/*
udanax-top.st:41867:Recipe methodsFor: 'protected: creation'!
create: cat {Category} with: cuisine {Recipe star vector}
	"cuisine points to the *variable* in which the receiver should be registered."
	
	super create.
	myCat _ cat.
	myNext _ cuisine refValue.
	cuisine refAssign: self!
*/
}
public static void linkTimeNonInherited() {
	Recipe.defineGlobal(XPP_CUISINE, null);
/*
udanax-top.st:41887:Recipe class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	Recipe star defineGlobal: #XppCuisine with: NULL.!
*/
}
/**
 * Get a cuisine name from a directory name.
 */
public static String mapCuisine(CxxClassDescription cxc) {
	String cuisine;
	cuisine = cxc.fetchAttribute(COPY);
	if (cuisine == null) {
		Association d = (Association) cxc.fetchDirectory();
		cuisine = (d != null ) ? d.key() : "Xanadu";
	}
	cuisine = cuisine;
	if (cuisine == "xlatexpp") {
		return "XppCuisine";
	}
	if (cuisine == "comm") {
		return "XppCuisine";
	}
	if (cuisine == "server") {
		return "DiskCuisine";
	}
	if (cuisine == "spires") {
		return "SpireCuisine";
	}
	return AboraSupport.asCapitalized(cuisine) + "Cuisine";
/*
udanax-top.st:41890:Recipe class methodsFor: 'smalltalk: init'!
{void} mapCuisine: cxc {CxxClassDescription}
	"Get a cuisine name from a directory name."
	| cuisine {char star} |
	cuisine _ cxc fetchAttribute: #COPY.
	cuisine == NULL ifTrue: [cuisine _ cxc fetchDirectory notNil: [:d | d key] else: ['Xanadu']].
	cuisine := cuisine asString.
	cuisine = 'xlatexpp' ifTrue: [^'XppCuisine'].
	cuisine = 'comm' ifTrue: [^'XppCuisine'].
	cuisine = 'server' ifTrue: [^'DiskCuisine'].
	cuisine = 'spires' ifTrue: [^'SpireCuisine'].
	^cuisine asString asCapitalized, 'Cuisine'!
*/
}
public static void staticTimeNonInherited() {
	OrderedCollection allSubclasses = AboraSupport.allSubclasses(Heaper.class);
	for (int doIndex = 0; doIndex < allSubclasses.size(); doIndex ++ ) {
		AboraClass cls = (AboraClass) allSubclasses.get(doIndex);
		CxxClassDescription cxc = (CxxClassDescription) cls.fetchCxxClassDescription();
		if (cxc != null ) {
			if ((cxc.includesAttribute(COPY)) && ( ! (cxc.includesAttribute(MANUALRECIPE)) && (cxc.includesAttribute(CONCRETE)))) {
				new ActualCopyRecipe(cls, (Smalltalk.associationAtIfAbsent((mapCuisine(cxc)), new Association())));
			}
			if ((cxc.includesAttribute(PSEUDOCOPY)) && (cxc.includesAttribute(CONCRETE))) {
				new PseudoCopyRecipe(cls, (Smalltalk.associationAtIfAbsent((mapCuisine(cxc)), new Association())));
			}
			/* (cxc includesAttribute: #BY.PROXY)
				ifTrue: 
					[| proxy |
					proxy _ Smalltalk at: (cls name, 'Proxy') asSymbol ifAbsent: [].
					proxy ~~ nil ifTrue:
						[ActualProxyRecipe 
							create: proxy
							with: 
								(Smalltalk associationAt: (self mapCuisine: cxc) asSymbol 
									ifAbsent: [Association new])]] */
			;
		}
	}
/*
udanax-top.st:41903:Recipe class methodsFor: 'smalltalk: init'!
{void} staticTimeNonInherited
	[Heaper allSubclassesDo: 
		[:cls {Behavior} |
		cls fetchCxxClassDescription notNil:
			[:cxc |
			((cxc includesAttribute: #COPY)
				and: [(cxc includesAttribute: #MANUAL.RECIPE) not
				and: [cxc includesAttribute: #CONCRETE]])
				ifTrue: 
					[ActualCopyRecipe create: cls with: 
						(Smalltalk associationAt: (self mapCuisine: cxc) asSymbol 
								ifAbsent: [Association new])].
			((cxc includesAttribute: #PSEUDO.COPY)
				and: [cxc includesAttribute: #CONCRETE])
				ifTrue: 
					[PseudoCopyRecipe create: cls with: 
						(Smalltalk associationAt: (self mapCuisine: cxc) asSymbol 
								ifAbsent: [Association new])].			
			"(cxc includesAttribute: #BY.PROXY)
				ifTrue: 
					[| proxy |
					proxy _ Smalltalk at: (cls name, 'Proxy') asSymbol ifAbsent: [].
					proxy ~~ nil ifTrue:
						[ActualProxyRecipe 
							create: proxy
							with: 
								(Smalltalk associationAt: (self mapCuisine: cxc) asSymbol 
									ifAbsent: [Association new])]]"]]] smalltalkOnly!
*/
}
public Recipe() {
/*

Generated during transformation
*/
}
public Recipe(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
public static void defineGlobal(String globalName, Heaper initialValue) {
	AboraSupport.defineGlobalRecipe(globalName, initialValue);
/*

Generated during transformation: AddMethod
*/
}
}
