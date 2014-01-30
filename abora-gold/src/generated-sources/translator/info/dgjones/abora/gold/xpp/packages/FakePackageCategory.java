/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xpp.packages;

import info.dgjones.abora.gold.java.missing.smalltalk.AboraClass;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import info.dgjones.abora.gold.xpp.packages.FakeCategory;
import info.dgjones.abora.gold.xpp.packages.Package;
import java.io.PrintWriter;

public class FakePackageCategory extends FakeCategory {

	protected Category myContentsCategory;
	protected Category myOriginalContentsCategory;
/*
Xanadu-Xpp-Packages.st:168:
FakeCategory subclass: #FakePackageCategory
	instanceVariableNames: '
		myContentsCategory {Category}
		myOriginalContentsCategory {Category}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xpp-Packages'!
*/
public FakePackageCategory(String name, AboraClass original, Category parent, Category originalContents) {
	super(name, original, parent);
	myContentsCategory = null;
	myOriginalContentsCategory = originalContents;
/*
Xanadu-Xpp-Packages.st:180:FakePackageCategory methodsFor: 'create'!
create: name {String} with: original {Class} with: parent {Category}
	with: originalContents {Category}
	
	super create: name with: original with: parent.
	myContentsCategory _ NULL.
	myOriginalContentsCategory _ originalContents.!
*/
}
public Category contentsCategory() {
	return myContentsCategory;
/*
Xanadu-Xpp-Packages.st:189:FakePackageCategory methodsFor: 'packages'!
contentsCategory
	^myContentsCategory!
*/
}
public boolean isConcretePackage() {
	return myOriginalClass.isConcretePackage();
/*
Xanadu-Xpp-Packages.st:192:FakePackageCategory methodsFor: 'packages'!
isConcretePackage
	^myOriginalClass isConcretePackage!
*/
}
/**
 * make a hooked 'instance' of this class
 */
public Heaper makeHooked() {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
Xanadu-Xpp-Packages.st:195:FakePackageCategory methodsFor: 'packages'!
{Package} makeHooked
	"make a hooked 'instance' of this class"
	^self contentsCategory makeHooked getOrMakePackage: self!
*/
}
/**
 * make a new package with the specified contents
 */
public Package makePackage(Heaper contents) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
Xanadu-Xpp-Packages.st:199:FakePackageCategory methodsFor: 'packages'!
{Package} makePackage: contents {Heaper}
	"make a new package with the specified contents"
	^myOriginalClass new create: contents;
		packagingCategory: self;
		constructPackage;
		yourself!
*/
}
public Category originalContentsCategory() {
	return myOriginalContentsCategory;
/*
Xanadu-Xpp-Packages.st:206:FakePackageCategory methodsFor: 'packages'!
originalContentsCategory
	^myOriginalContentsCategory!
*/
}
public void registerWithContents() {
	if (myContentsCategory == null) {
		myContentsCategory = myOriginalContentsCategory.registerPackageCategory(this);
	}
/*
Xanadu-Xpp-Packages.st:209:FakePackageCategory methodsFor: 'packages'!
{void} registerWithContents
	myContentsCategory == NULL ifTrue:
		[myContentsCategory := myOriginalContentsCategory registerPackageCategory: self]!
*/
}
public void showOn(PrintWriter oo) {
	oo.print(name());
	oo.print(" [");
	oo.print(contentsCategory().name());
	oo.print("]");
/*
Xanadu-Xpp-Packages.st:215:FakePackageCategory methodsFor: 'inspecting'!
showOn: oo
	oo << self name << ' [' << self contentsCategory inspectString << ']'!
*/
}
public FakePackageCategory() {
/*

Generated during transformation
*/
}
public FakePackageCategory(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
