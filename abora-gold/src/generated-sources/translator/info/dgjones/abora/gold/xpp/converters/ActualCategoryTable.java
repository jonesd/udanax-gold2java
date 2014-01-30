/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xpp.converters;

import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Behavior;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.primtab.PrimPtr2PtrTable;
import info.dgjones.abora.gold.primtab.PrimPtr2PtrTableStepper;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import info.dgjones.abora.gold.xpp.converters.ActualCategoryTable;
import info.dgjones.abora.gold.xpp.converters.CategoryTable;
import java.io.PrintWriter;

public class ActualCategoryTable extends CategoryTable {

	protected PrimPtr2PtrTable myTable;
/*
Xanadu-Xpp-Converters.st:188:
CategoryTable subclass: #ActualCategoryTable
	instanceVariableNames: 'myTable {PrimPtr2PtrTable}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xpp-Converters'!
*/
/*
Xanadu-Xpp-Converters.st:194:
(ActualCategoryTable getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ActualCategoryTable.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * add a new value, blasting if it is already there
 */
public void introduce(Category category, Heaper value) {
	myTable.introduce(category, value);
/*
Xanadu-Xpp-Converters.st:199:ActualCategoryTable methodsFor: 'category accessing'!
{void} at: category {Category} introduce: value {Heaper}
	"add a new value, blasting if it is already there"
	myTable at: category introduce: value.!
*/
}
/**
 * find the contents at any subclass of this category
 */
public Heaper fetchSubOf(Category category) {
	Heaper result;
	PrimPtr2PtrTableStepper stepper;
	result = myTable.fetch(category);
	if (result != null) {
		return result;
	}
	stepper = myTable.stepper();
	while (stepper.hasValue()) {
		Heaper aValue;
		aValue = stepper.get();
		if (((Behavior) stepper.heaperKey()).isEqualOrSubclassOf(category)) {
			return aValue;
		}
		stepper.step();
	}
	return null;
/*
Xanadu-Xpp-Converters.st:203:ActualCategoryTable methodsFor: 'category accessing'!
{Heaper} fetchSubOf: category {Category}
	"find the contents at any subclass of this category"
	| result {Heaper} stepper {PrimPtr2PtrTableStepper} |
	result _ myTable fetch: category.
	result ~~ NULL ifTrue:
		[^result].
	stepper _ myTable stepper.
	[stepper hasValue] whileTrue: [ | aValue {Heaper wimpy} |
		aValue _ stepper get.
		((stepper heaperKey cast: Behavior) 
			isEqualOrSubclassOf: category) ifTrue: [^aValue].
		stepper step].
	^NULL!
*/
}
/**
 * find the contents at the nearest superclass of this category
 */
public Heaper fetchSuperOf(Category category) {
	Category parent;
	parent = category;
	while (parent != null) {
		Heaper result;
		result = myTable.fetch(parent);
		if (result != null) {
			return result;
		}
		parent = parent.fetchSuperCategory();
	}
	return null;
/*
Xanadu-Xpp-Converters.st:217:ActualCategoryTable methodsFor: 'category accessing'!
{Heaper} fetchSuperOf: category {Category}
	"find the contents at the nearest superclass of this category"
	| parent {Category} |
	parent _ category.
	[parent ~~ NULL] whileTrue:
		[ | result {Heaper} |
		result _ myTable fetch: parent.
		result ~~ NULL ifTrue:
			[^result].
		parent _ parent fetchSuperCategory].
	^NULL!
*/
}
/**
 * store the contents of another category table into this one
 */
public void storeAll(CategoryTable other) {
	PrimPtr2PtrTableStepper keys;
	Stepper stomper = (keys = ((ActualCategoryTable) other).table().stepper());
	for (; stomper.hasValue(); stomper.step()) {
		Heaper value = (Heaper) stomper.fetch();
		if (value == null) {
			continue ;
		}
		myTable.store(keys.heaperKey(), value);
	}
	stomper.destroy();
/*
Xanadu-Xpp-Converters.st:229:ActualCategoryTable methodsFor: 'category accessing'!
{void} storeAll: other {CategoryTable}
	"store the contents of another category table into this one"
	| keys {PrimPtr2PtrTableStepper} |
	(keys _ (other cast: ActualCategoryTable) table stepper) forEach:
		[ :value {Heaper} |
			myTable at: keys heaperKey store: value]!
*/
}
public PrimPtr2PtrTable table() {
	return myTable;
/*
Xanadu-Xpp-Converters.st:236:ActualCategoryTable methodsFor: 'category accessing'!
{PrimPtr2PtrTable} table
	^myTable!
*/
}
public ActualCategoryTable() {
	super();
	myTable = PrimPtr2PtrTable.make(16);
/*
Xanadu-Xpp-Converters.st:241:ActualCategoryTable methodsFor: 'creation'!
create
	super create.
	myTable _ PrimPtr2PtrTable make: 16.!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	/* myTable printOnWithSyntax: oo with: '[' with: ', ' with: ']' */
/*
Xanadu-Xpp-Converters.st:247:ActualCategoryTable methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name.
	"myTable printOnWithSyntax: oo with: '[' with: ', ' with: ']'"!
*/
}
public ActualCategoryTable(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
