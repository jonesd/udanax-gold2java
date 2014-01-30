/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xpp.packages;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.PackageCategory;
import info.dgjones.abora.gold.java.missing.smalltalk.AboraClass;
import info.dgjones.abora.gold.java.missing.smalltalk.Smalltalk;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import info.dgjones.abora.gold.xpp.packages.Package;
import java.io.PrintWriter;

public class Package extends Heaper {

	protected Heaper myContents;
	protected static Category myContentsCategory;
	protected static Category myOriginalContentsCategory;
/*
Xanadu-Xpp-Packages.st:0:
Heaper subclass: #Package
	instanceVariableNames: 'myContents {Heaper smalltalk}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xpp-Packages'!
*/
/*
Xanadu-Xpp-Packages.st:26:
Package class
	instanceVariableNames: 'myContentsCategory {Category star smalltalk} myOriginalContentsCategory {Category star smalltalk} '!
*/
/**
 * a construct message that gets sent when each package object is created
 * should always call super constructPackage at the beginning
 */
public void constructPackage() {
/*
Xanadu-Xpp-Packages.st:8:Package methodsFor: 'create'!
{void} constructPackage
	"a construct message that gets sent when each package object is created
	should always call super constructPackage at the beginning"!
*/
}
public Package(Heaper contents) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
Xanadu-Xpp-Packages.st:12:Package methodsFor: 'create'!
create: contents {Heaper}
	super create.
	myContents _ contents.
	myPackagingCategory _ self class!
*/
}
public Heaper contents() {
	return myContents;
/*
Xanadu-Xpp-Packages.st:19:Package methodsFor: 'accessing'!
{Heaper} contents
	^myContents!
*/
}
public Heaper getContents() {
	return myContents;
/*
Xanadu-Xpp-Packages.st:22:Package methodsFor: 'accessing'!
{Heaper} getContents
	^myContents!
*/
}
/**
 * this is a hack so that this happens before any actual code gets run
 */
public static void linkTimeInherited() {
	AboraSupport.smalltalkOnly();
	{
		myOriginalContentsCategory = null;
		/* access method will pull from attributes */
		myContentsCategory = null;
	}
/*
Xanadu-Xpp-Packages.st:33:Package class methodsFor: 'smalltalk: init'!
linkTimeInherited
	"this is a hack so that this happens before any actual code gets run"
	[myOriginalContentsCategory _ NULL. "access method will pull from attributes"
	myContentsCategory _ NULL] smalltalkOnly!
*/
}
public static void suppressLinkTimeInherited() {
/*
Xanadu-Xpp-Packages.st:39:Package class methodsFor: 'smalltalk: init'!
suppressLinkTimeInherited!
*/
}
public static Category contentsCategory() {
	registerWithContents();
	return myContentsCategory;
/*
Xanadu-Xpp-Packages.st:43:Package class methodsFor: 'packages'!
{Category} contentsCategory
	self registerWithContents.
	^myContentsCategory!
*/
}
/**
 * whether this package should be instantiated
 */
public static boolean isConcretePackage() {
	return hasAttribute(CONCRETE);
/*
Xanadu-Xpp-Packages.st:47:Package class methodsFor: 'packages'!
{BooleanVar} isConcretePackage
	"whether this package should be instantiated"
	^self hasAttribute: #CONCRETE!
*/
}
public static Package MAKEUHOOKED(String packageName) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
Xanadu-Xpp-Packages.st:51:Package class methodsFor: 'packages'!
{Package} MAKE.U.HOOKED: packageName {Symbol}
	^(Smalltalk at: packageName) makeHooked!
*/
}
/**
 * make a hooked instance of this class with the given packaging category
 */
public static Package makeHooked() {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
Xanadu-Xpp-Packages.st:54:Package class methodsFor: 'packages'!
{Package} makeHooked
	"make a hooked instance of this class with the given packaging category"
	^self contentsCategory makeHooked getOrMakePackage: self!
*/
}
/**
 * make a new package with the specified contents
 */
public static Package makePackage(Heaper contents) {
	Package result;
	result = new Package(contents);
	result.constructPackage();
	return result;
/*
Xanadu-Xpp-Packages.st:58:Package class methodsFor: 'packages'!
{Package} makePackage: contents {Heaper}
	"make a new package with the specified contents"
	| result {Heaper} |
	result _ self create: contents.
	result constructPackage.
	^result!
*/
}
public static AboraClass originalClass() {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
Xanadu-Xpp-Packages.st:65:Package class methodsFor: 'packages'!
originalClass
	^self!
*/
}
public static Category originalContentsCategory() {
	if (myOriginalContentsCategory == null || (myOriginalContentsCategory == null)) {
		String original;
		original = fetchAttribute(PACKAGE);
		if (original == null) {
			original = fetchAttribute(PACKAGEHOOK);
		}
		myOriginalContentsCategory = Smalltalk.at(original);
	}
	return myOriginalContentsCategory;
/*
Xanadu-Xpp-Packages.st:68:Package class methodsFor: 'packages'!
{Category} originalContentsCategory
	(myOriginalContentsCategory == NULL or: [myOriginalContentsCategory == nil]) ifTrue:
		[ | original {Symbol} |
		original := self fetchAttribute: #PACKAGE.
		original == NULL ifTrue:
			[original := self fetchAttribute: #PACKAGE.HOOK].
		myOriginalContentsCategory := Smalltalk at: original].
	^myOriginalContentsCategory!
*/
}
/**
 * a package category can be added; remember it and return what its contents class should be
 */
public static Category registerPackageCategory(PackageCategory packageCat) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
Xanadu-Xpp-Packages.st:77:Package class methodsFor: 'packages'!
{Category} registerPackageCategory: packageCat {PackageCategory}
	"a package category can be added; remember it and return what its contents class should be"
	(self hasAttribute: #PACKAGE.HOOK)
		ifTrue: [^(FakePackageCategory create: packageCat name , '_' , self name
				with: self
				with: (packageCat getSuperCategory == Package
					ifTrue: [self getSuperCategory]
					ifFalse: [packageCat getSuperCategory contentsCategory])
				with: self originalContentsCategory)
			addPackageCategory: packageCat;
			registerWithContents;
			yourself]
		ifFalse: [self addPackageCategory: packageCat.
			^self]!
*/
}
/**
 * tell my contents category that I am a package on it
 */
public static void registerWithContents() {
	if (myContentsCategory == null) {
		myContentsCategory = originalContentsCategory().registerPackageCategory(Package.class);
	}
/*
Xanadu-Xpp-Packages.st:92:Package class methodsFor: 'packages'!
{void} registerWithContents
	"tell my contents category that I am a package on it"
	myContentsCategory == NULL ifTrue:
		[myContentsCategory _ self originalContentsCategory registerPackageCategory: self]!
*/
}
public static void showOn(PrintWriter oo) {
	oo.print(Package.class.getName());
	oo.print(" [");
	oo.print(contentsCategory().name());
	oo.print("]");
/*
Xanadu-Xpp-Packages.st:97:Package class methodsFor: 'packages'!
showOn: oo
	oo << self name << ' [' << self contentsCategory name << ']'!
*/
}
public Package() {
/*

Generated during transformation
*/
}
public Package(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
public static String fetchAttribute(String attributeName) {
	throw new UnsupportedOperationException();
/*

Generated during transformation: AddMethod
*/
}
public static boolean hasAttribute(String attributeName) {
	throw new UnsupportedOperationException();
/*

Generated during transformation: AddMethod
*/
}
}
