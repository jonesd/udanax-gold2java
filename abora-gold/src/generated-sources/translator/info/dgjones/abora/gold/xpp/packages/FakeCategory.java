/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xpp.packages;

import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.PackageCategory;
import info.dgjones.abora.gold.java.missing.smalltalk.AboraClass;
import info.dgjones.abora.gold.java.missing.smalltalk.OrderedCollection;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class FakeCategory extends Heaper {

	protected String myName;
	protected AboraClass myOriginalClass;
	protected Category mySuperCategory;
	protected OrderedCollection myPackageClasses;
/*
Xanadu-Xpp-Packages.st:100:
Heaper subclass: #FakeCategory
	instanceVariableNames: '
		myName {String}
		myOriginalClass {Class}
		mySuperCategory {Category}
		myPackageClasses {OrderedCollection of: Category}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xpp-Packages'!
*/
public FakeCategory(String name, AboraClass original, Category parent) {
	super();
	myName = name;
	myOriginalClass = original;
	mySuperCategory = parent;
	myPackageClasses = new OrderedCollection();
/*
Xanadu-Xpp-Packages.st:114:FakeCategory methodsFor: 'create'!
create: name {String} with: original {Class} with: parent {Category}
	super create.
	myName := name.
	myOriginalClass := original.
	mySuperCategory := parent.
	myPackageClasses := OrderedCollection new.!
*/
}
public void addPackageCategory(Category pCat) {
	myPackageClasses.add(pCat);
/*
Xanadu-Xpp-Packages.st:123:FakeCategory methodsFor: 'packages'!
{void} addPackageCategory: pCat {Category}
	myPackageClasses add: pCat!
*/
}
/**
 * make a hooked 'instance' of this class
 */
public Heaper makeHooked() {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
Xanadu-Xpp-Packages.st:126:FakeCategory methodsFor: 'packages'!
{Heaper} makeHooked
	"make a hooked 'instance' of this class"
	^(myOriginalClass new create) packagingCategory: self; yourself!
*/
}
public OrderedCollection packageClasses() {
	return myPackageClasses;
/*
Xanadu-Xpp-Packages.st:130:FakeCategory methodsFor: 'packages'!
{OrderedCollection of: PackageCategory} packageClasses
	^myPackageClasses!
*/
}
public Category registerPackageCategory(PackageCategory packageCat) {
	throw new AboraRuntimeException("Attempted to add a package to a hook-generated category");
/*
Xanadu-Xpp-Packages.st:133:FakeCategory methodsFor: 'packages'!
{Category} registerPackageCategory: packageCat {PackageCategory}
	self error: 'Attempted to add a package to a hook-generated category'!
*/
}
/**
 * get information attached to the given attribute, or NULL if none
 */
public String fetchAttribute(Object attr) {
	return myOriginalClass.fetchAttribute(attr);
/*
Xanadu-Xpp-Packages.st:138:FakeCategory methodsFor: 'accessing'!
{Symbol} fetchAttribute: attr
	"get information attached to the given attribute, or NULL if none"
	^myOriginalClass fetchAttribute: attr!
*/
}
public Category getSuperCategory() {
	return mySuperCategory;
/*
Xanadu-Xpp-Packages.st:142:FakeCategory methodsFor: 'accessing'!
getSuperCategory
	^mySuperCategory!
*/
}
/**
 * check if the attributes information includes this
 */
public boolean hasAttribute(Object attr) {
	return myOriginalClass.hasAttribute(attr);
/*
Xanadu-Xpp-Packages.st:145:FakeCategory methodsFor: 'accessing'!
{BooleanVar} hasAttribute: attr
	"check if the attributes information includes this"
	^myOriginalClass hasAttribute: attr!
*/
}
public boolean isEqualOrSubclassOf(Category cat) {
	return myOriginalClass.isEqualOrSubclassOf(cat.originalClass());
/*
Xanadu-Xpp-Packages.st:149:FakeCategory methodsFor: 'accessing'!
isEqualOrSubclassOf: cat {Category}
	^myOriginalClass isEqualOrSubclassOf: cat originalClass!
*/
}
public String name() {
	return myName;
/*
Xanadu-Xpp-Packages.st:152:FakeCategory methodsFor: 'accessing'!
{String} name
	^myName!
*/
}
public AboraClass originalClass() {
	return myOriginalClass;
/*
Xanadu-Xpp-Packages.st:155:FakeCategory methodsFor: 'accessing'!
originalClass
	^myOriginalClass!
*/
}
public void withAllSuperclasses() {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
Xanadu-Xpp-Packages.st:158:FakeCategory methodsFor: 'accessing'!
withAllSuperclasses
	^self getSuperCategory withAllSuperclasses
		addFirst: self;
		yourself!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print('(');
	oo.print(name());
	oo.print(')');
/*
Xanadu-Xpp-Packages.st:165:FakeCategory methodsFor: 'printing'!
printOn: oo
	oo << self getCategory name << $( << self name << $)!
*/
}
public FakeCategory() {
/*

Generated during transformation
*/
}
public FakeCategory(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
