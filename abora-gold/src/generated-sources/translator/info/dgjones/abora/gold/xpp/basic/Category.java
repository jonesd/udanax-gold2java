/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xpp.basic;

import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.AboraClass;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class Category extends Heaper {

	protected AboraClass myClass;
/*
Xanadu-Xpp-Basic.st:2947:
Heaper subclass: #Category
	instanceVariableNames: 'myClass {UNKNOWN}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xpp-Basic'!
*/
/*
Xanadu-Xpp-Basic.st:2983:
Category class
	instanceVariableNames: ''!
*/
public AboraClass brotherClass() {
	return myClass;
/*
Xanadu-Xpp-Basic.st:2957:Category methodsFor: 'accessing'!
brotherClass
	^myClass!
*/
}
public Category(AboraClass cat) {
	super();
	myClass = cat;
/*
Xanadu-Xpp-Basic.st:2960:Category methodsFor: 'accessing'!
create: cat 
	super create.
	myClass _ cat!
*/
}
/*
Xanadu-Xpp-Basic.st:2964:Category methodsFor: 'accessing'!
getCategory
	^Category create: Category!
*/
public boolean inheritsFrom(Object cat) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
Xanadu-Xpp-Basic.st:2967:Category methodsFor: 'accessing'!
inheritsFrom: cat
	cat class == Category ifTrue: [^myClass == cat brotherClass or: [myClass inheritsFrom: cat brotherClass]]
		ifFalse: [^myClass == cat or: [myClass inheritsFrom: cat]]!
*/
}
public String name() {
	return myClass.name();
/*
Xanadu-Xpp-Basic.st:2971:Category methodsFor: 'accessing'!
name
	^myClass name!
*/
}
public int preorderNumber() {
	return myClass.preorderNumber();
/*
Xanadu-Xpp-Basic.st:2974:Category methodsFor: 'accessing'!
preorderNumber
	^myClass preorderNumber!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	xmtr.sendString(name());
/*
Xanadu-Xpp-Basic.st:2979:Category methodsFor: 'stubble'!
sendSelfTo: xmtr
	xmtr sendString: self name!
*/
}
public static void create(Rcvr rcvr) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
Xanadu-Xpp-Basic.st:2990:Category class methodsFor: 'stubble'!
create.Rcvr: rcvr
	| catName |
	catName _ rcvr receiveString.
	^Heaper findCategory: catName!
*/
}
/**
 * translate a category name into a class
 */
public static Category find(UInt8Array catName) {
	/* Transform: Rewrote body */
	return AboraSupport.findCategory(catName.toString());
/*
Xanadu-Xpp-Basic.st:2997:Category class methodsFor: 'accessing'!
{Class} find: catName {UInt8Array}
	"translate a category name into a class"
	"Warning:  smalltalk wizardy."
	| oldCat result |
	catName isSymbol 
		ifTrue: [^Smalltalk at: catName
			ifAbsent: [Heaper BLAST: #'CATEGORY_NOT_FOUND']].
	oldCat _ catName class.
	catName changeClassToThatOf: ''.
	result _ Smalltalk at: catName asSymbol 
		ifAbsent: [Heaper BLAST: #'CATEGORY_NOT_FOUND'].
	catName changeClassToThatOf: oldCat basicNew.
	^result!
*/
}
public Category() {
/*

Generated during transformation
*/
}
public Category(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
public Category makeHooked() {
	throw new UnsupportedOperationException();
/*

Generated during transformation: AddMethod
*/
}
public Category fetchSuperCategory() {
	throw new UnsupportedOperationException();
/*

Generated during transformation: AddMethod
*/
}
public Category registerPackageCategory(Object packageCategory) {
	throw new UnsupportedOperationException();
/*

Generated during transformation: AddMethod
*/
}
public AboraClass originalClass() {
	throw new UnsupportedOperationException();
/*

Generated during transformation: AddMethod
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*

Generated during transformation: AddMethod
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(name());
	oo.print(")");
/*

Generated during transformation: AddMethod
*/
}
}
