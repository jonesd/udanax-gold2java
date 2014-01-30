/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.cobbler;

import info.dgjones.abora.gold.cobbler.Connection;
import info.dgjones.abora.gold.cobbler.DirectConnection;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * We just made the object, so the connection is just a reference to the object.
 */
public class DirectConnection extends Connection {

	protected Category myCategory;
	protected Heaper myHeaper;
/*
udanax-top.st:13914:
Connection subclass: #DirectConnection
	instanceVariableNames: '
		myCategory {Category}
		myHeaper {Heaper}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-cobbler'!
*/
/*
udanax-top.st:13920:
DirectConnection comment:
'We just made the object, so the connection is just a reference to the object.'!
*/
/*
udanax-top.st:13922:
(DirectConnection getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(DirectConnection.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Category bootCategory() {
	return myCategory;
/*
udanax-top.st:13927:DirectConnection methodsFor: 'accessing'!
{Category} bootCategory
	^myCategory!
*/
}
public Heaper bootHeaper() {
	return myHeaper;
/*
udanax-top.st:13930:DirectConnection methodsFor: 'accessing'!
{Heaper} bootHeaper
	^myHeaper!
*/
}
public DirectConnection(Category cat, Heaper heaper) {
	super();
	myCategory = cat;
	myHeaper = heaper;
/*
udanax-top.st:13935:DirectConnection methodsFor: 'creation'!
create: cat {Category} with: heaper {Heaper}
	super create.
	myCategory _ cat.
	myHeaper _ heaper!
*/
}
/**
 * myHeaper destroy. There are bootHeapers that you REALLY don't want to destroy, such as the
 * GrandMap
 */
public void destruct() {
	super.destruct();
/*
udanax-top.st:13940:DirectConnection methodsFor: 'creation'!
{void} destruct
	"myHeaper destroy. There are bootHeapers that you REALLY don't want to destroy, such as the GrandMap"
	super destruct!
*/
}
public DirectConnection() {
/*

Generated during transformation
*/
}
public DirectConnection(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
