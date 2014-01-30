/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xpp.converters;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.OccludingCategoryTable;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import info.dgjones.abora.gold.xpp.converters.ActualCategoryTable;
import info.dgjones.abora.gold.xpp.converters.CategoryTable;

public class CategoryTable extends Heaper {

/*
Xanadu-Xpp-Converters.st:117:
Heaper subclass: #CategoryTable
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xpp-Converters'!
*/
/*
Xanadu-Xpp-Converters.st:123:
(CategoryTable getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
Xanadu-Xpp-Converters.st:174:
CategoryTable class
	instanceVariableNames: ''!
*/
/*
Xanadu-Xpp-Converters.st:177:
(CategoryTable getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CategoryTable.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * find the contents at any subclass of this category
 */
public Heaper fetchSubOf(Category category) {
	throw new SubclassResponsibilityException();
/*
Xanadu-Xpp-Converters.st:128:CategoryTable methodsFor: 'deferred category accessing'!
{Heaper} fetchSubOf: category {Category}
	"find the contents at any subclass of this category"
	self subclassResponsibility!
*/
}
/**
 * find the contents at the nearest superclass of this category
 */
public Heaper fetchSuperOf(Category category) {
	throw new SubclassResponsibilityException();
/*
Xanadu-Xpp-Converters.st:132:CategoryTable methodsFor: 'deferred category accessing'!
{Heaper} fetchSuperOf: category {Category}
	"find the contents at the nearest superclass of this category"
	self subclassResponsibility!
*/
}
/**
 * store the contents of another category table into this one
 */
public void storeAll(CategoryTable other) {
	throw new SubclassResponsibilityException();
/*
Xanadu-Xpp-Converters.st:136:CategoryTable methodsFor: 'deferred category accessing'!
{void} storeAll: other {CategoryTable}
	"store the contents of another category table into this one"
	self subclassResponsibility!
*/
}
/**
 * add a new value, blasting if it is already there
 */
public void introduce(Category category, Heaper value) {
	throw new AboraRuntimeException(AboraRuntimeException.FATAL_ERROR);
/*
Xanadu-Xpp-Converters.st:142:CategoryTable methodsFor: 'category accessing'!
{void} at: category {Category unused} introduce: value {Heaper unused}
	"add a new value, blasting if it is already there"
	Heaper BLAST: #FatalError.!
*/
}
/**
 * find the contents at any subclass of this category
 */
public Heaper getSubOf(Category category) {
	Heaper result;
	result = fetchSubOf(category);
	if (result == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
	return result;
/*
Xanadu-Xpp-Converters.st:146:CategoryTable methodsFor: 'category accessing'!
{Heaper} getSubOf: category {Category}
	"find the contents at any subclass of this category"
	| result {Heaper} |
	result _ self fetchSubOf: category.
	result == NULL ifTrue: [Heaper BLAST: #NotInTable].
	^result!
*/
}
/**
 * find the contents at the nearest superclass of this category
 */
public Heaper getSuperOf(Category category) {
	Heaper result;
	result = fetchSuperOf(category);
	if (result == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
	return result;
/*
Xanadu-Xpp-Converters.st:153:CategoryTable methodsFor: 'category accessing'!
{Heaper} getSuperOf: category {Category}
	"find the contents at the nearest superclass of this category"
	| result {Heaper} |
	result _ self fetchSuperOf: category.
	result == NULL ifTrue: [Heaper BLAST: #NotInTable].
	^result!
*/
}
public CategoryTable() {
	super();
/*
Xanadu-Xpp-Converters.st:162:CategoryTable methodsFor: 'creation'!
create
	super create.!
*/
}
public int hashForEqual() {
	return asOop();
/*
Xanadu-Xpp-Converters.st:167:CategoryTable methodsFor: 'testing'!
{UInt32} hashForEqual
	^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
Xanadu-Xpp-Converters.st:170:CategoryTable methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	^self == other!
*/
}
public static CategoryTable make() {
	return new ActualCategoryTable();
/*
Xanadu-Xpp-Converters.st:182:CategoryTable class methodsFor: 'pseudo constructors'!
make
	^ActualCategoryTable create!
*/
}
public static CategoryTable make(CategoryTable front, CategoryTable back) {
	return new OccludingCategoryTable(front, back);
/*
Xanadu-Xpp-Converters.st:185:CategoryTable class methodsFor: 'pseudo constructors'!
make: front {CategoryTable} with: back {CategoryTable}
	^OccludingCategoryTable create: front with: back!
*/
}
public CategoryTable(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
