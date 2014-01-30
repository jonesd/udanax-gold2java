/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.backend;

import info.dgjones.abora.gold.backend.DiskManagerEmulsion;
import info.dgjones.abora.gold.java.missing.smalltalk.Array;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.fluid.Emulsion;

public class DiskManagerEmulsion extends Emulsion {

/*
udanax-top.st:18140:
Emulsion subclass: #DiskManagerEmulsion
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-backend'!
*/
/*
udanax-top.st:18163:
DiskManagerEmulsion class
	instanceVariableNames: ''!
*/
public Array fetchNewRawSpace(int size) {
	/* Removed translateOnly */
	return ((DiskManager) CurrentPacker.fluidGet()).fluidSpace(( new Array(size)));
/*
udanax-top.st:18148:DiskManagerEmulsion methodsFor: 'accessing'!
{void star} fetchNewRawSpace: size {#size.U.t var}
	'return CurrentPacker.fluidGet()->fluidSpace( (char *) fcalloc (size, sizeof(char)) );' translateOnly.
	[^CurrentPacker fluidGet fluidSpace: (Array new: size)] smalltalkOnly!
*/
}
public Array fetchOldRawSpace() {
	return ((DiskManager) CurrentPacker.fluidGet()).fluidSpace();
/*
udanax-top.st:18153:DiskManagerEmulsion methodsFor: 'accessing'!
{void star} fetchOldRawSpace
	^CurrentPacker fluidGet fluidSpace!
*/
}
public DiskManagerEmulsion() {
	super();
/*
udanax-top.st:18159:DiskManagerEmulsion methodsFor: 'creation'!
create
	super create!
*/
}
public static DiskManagerEmulsion make() {
	return new DiskManagerEmulsion();
/*
udanax-top.st:18170:DiskManagerEmulsion class methodsFor: 'creation'!
make
	^ DiskManagerEmulsion new create!
*/
}
public DiskManagerEmulsion(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
