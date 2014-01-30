/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.cobbler;

import info.dgjones.abora.gold.cobbler.BootMaker;
import info.dgjones.abora.gold.cobbler.BootPlan;
import info.dgjones.abora.gold.cobbler.Connection;
import info.dgjones.abora.gold.cobbler.DirectConnection;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class BootMaker extends BootPlan {

/*
udanax-top.st:56841:
BootPlan subclass: #BootMaker
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-cobbler'!
*/
/*
udanax-top.st:56845:
(BootMaker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #NOT.A.TYPE; add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BootMaker.class).setAttributes( new Set().add("NOTATYPE").add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public Category bootCategory() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:56850:BootMaker methodsFor: 'accessing'!
{Category} bootCategory
	self subclassResponsibility!
*/
}
/**
 * Return the object representing the connection.  This gives the client a handle by which to
 * terminate the connection.
 */
public Connection connection() {
	return new DirectConnection(bootCategory(), bootHeaper());
/*
udanax-top.st:56853:BootMaker methodsFor: 'accessing'!
{Connection} connection
	"Return the object representing the connection.  This gives the client a handle by which to terminate the connection."
	
	^DirectConnection create: self bootCategory with: self bootHeaper!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
/*
udanax-top.st:56860:BootMaker methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name.!
*/
}
/**
 * Subclasses of maker only need to define the routine that makes the boot heaper.
 */
public Heaper bootHeaper() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:56865:BootMaker methodsFor: 'protected:'!
{Heaper} bootHeaper
	"Subclasses of maker only need to define the routine that makes the boot heaper."
	
	self subclassResponsibility!
*/
}
public BootMaker() {
/*

Generated during transformation
*/
}
public BootMaker(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
