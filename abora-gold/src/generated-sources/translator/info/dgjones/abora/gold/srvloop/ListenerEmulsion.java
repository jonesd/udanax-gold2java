/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.srvloop;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.smalltalk.Array;
import info.dgjones.abora.gold.srvloop.ListenerEmulsion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.fluid.Emulsion;

public class ListenerEmulsion extends Emulsion {

	protected Array defaultFluidSpace;
/*
udanax-top.st:27879:
Emulsion subclass: #ListenerEmulsion
	instanceVariableNames: 'defaultFluidSpace {char star}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-srvloop'!
*/
/*
udanax-top.st:27910:
ListenerEmulsion class
	instanceVariableNames: ''!
*/
public Array fetchNewRawSpace(int size) {
	if (CurrentChunk == null) {
		AboraSupport.translateOnly();
		{
			/* cxx: return (defaultFluidSpace = (char *) fcalloc (size, sizeof(char))); */
			;
		}
		AboraSupport.smalltalkOnly();
		{
			return defaultFluidSpace = new Array(size);
		}
	}
	else {
		AboraSupport.translateOnly();
		{
			/* cxx: return CurrentChunk->fluidSpace( (char *) fcalloc (size, sizeof(char)) ); */
			;
		}
		AboraSupport.smalltalkOnly();
		{
			return CurrentChunk.fluidSpace(( new Array(size)));
		}
	}
/*
udanax-top.st:27887:ListenerEmulsion methodsFor: 'accessing'!
{void star} fetchNewRawSpace: size {#size.U.t var}
	(CurrentChunk == NULL) ifTrue: [
		["cxx: return (defaultFluidSpace = (char *) fcalloc (size, sizeof(char)));"] translateOnly.
		[^defaultFluidSpace _ Array new: size] smalltalkOnly]
	ifFalse: [
		["cxx: return CurrentChunk->fluidSpace( (char *) fcalloc (size, sizeof(char)) );"] translateOnly.
		[^CurrentChunk fluidSpace: (Array new: size)] smalltalkOnly]!
*/
}
public Array fetchOldRawSpace() {
	if (CurrentChunk == null) {
		return defaultFluidSpace;
	}
	else {
		return CurrentChunk.fluidSpace();
	}
/*
udanax-top.st:27896:ListenerEmulsion methodsFor: 'accessing'!
{void star} fetchOldRawSpace
	(CurrentChunk == NULL) ifTrue: [
		^defaultFluidSpace. ]
	ifFalse: [
		^CurrentChunk fluidSpace.]!
*/
}
public ListenerEmulsion() {
	super();
	defaultFluidSpace = null;
/*
udanax-top.st:27905:ListenerEmulsion methodsFor: 'creation'!
create
	super create.
	defaultFluidSpace _ NULL.!
*/
}
/**
 * @deprecated
 */
public static ListenerEmulsion make() {
	throw new PasseException();
/*
udanax-top.st:27917:ListenerEmulsion class methodsFor: 'smalltalk: passe'!
make
	
	self passe. "use 'Listener listenerEmulsion'"!
*/
}
public ListenerEmulsion(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
