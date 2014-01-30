/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xpp.fluid;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Array;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.fluid.Emulsion;
import info.dgjones.abora.gold.xpp.fluid.GlobalEmulsion;

public class GlobalEmulsion extends Emulsion {

	protected Array myFluidsSpace;
	protected static Emulsion TheGlobalEmulsion;
/*
Xanadu-Xpp-fluid.st:103:
Emulsion subclass: #GlobalEmulsion
	instanceVariableNames: 'myFluidsSpace {Array of: Object}'
	classVariableNames: 'TheGlobalEmulsion {Emulsion} '
	poolDictionaries: ''
	category: 'Xanadu-Xpp-fluid'!
*/
/*
Xanadu-Xpp-fluid.st:110:
(GlobalEmulsion getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
Xanadu-Xpp-fluid.st:131:
GlobalEmulsion class
	instanceVariableNames: ''!
*/
/*
Xanadu-Xpp-fluid.st:134:
(GlobalEmulsion getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(GlobalEmulsion.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public Array fetchNewRawSpace(int size) {
	/* Removed translateOnly */
	return myFluidsSpace = new Array(size);
/*
Xanadu-Xpp-fluid.st:115:GlobalEmulsion methodsFor: 'accessing'!
{void star} fetchNewRawSpace: size {#size.U.t var}
	["cxx: return (myFluidsSpace = (char *) calloc (size, sizeof(char)));"] translateOnly.
	[^myFluidsSpace _ Array new: size] smalltalkOnly!
*/
}
public Array fetchOldRawSpace() {
	return myFluidsSpace;
/*
Xanadu-Xpp-fluid.st:120:GlobalEmulsion methodsFor: 'accessing'!
{void star} fetchOldRawSpace
	^myFluidsSpace!
*/
}
public GlobalEmulsion() {
	super();
	myFluidsSpace = null;
/*
Xanadu-Xpp-fluid.st:126:GlobalEmulsion methodsFor: 'create'!
create
	super create.
	myFluidsSpace _ NULL!
*/
}
public static Emulsion make() {
	if (TheGlobalEmulsion == null || (TheGlobalEmulsion == null)) {
		TheGlobalEmulsion = new GlobalEmulsion();
	}
	return TheGlobalEmulsion;
/*
Xanadu-Xpp-fluid.st:139:GlobalEmulsion class methodsFor: 'make'!
make
	(TheGlobalEmulsion == nil or: [TheGlobalEmulsion == NULL]) ifTrue:
		[TheGlobalEmulsion _ self new create].
	^ TheGlobalEmulsion!
*/
}
public static Emulsion globalEmulsion() {
	return make();
/*
Xanadu-Xpp-fluid.st:146:GlobalEmulsion class methodsFor: 'global: make'!
globalEmulsion
	^ self make!
*/
}
public static void cleanupGarbage() {
	TheGlobalEmulsion = null;
/*
Xanadu-Xpp-fluid.st:151:GlobalEmulsion class methodsFor: 'cleanup'!
cleanupGarbage
	TheGlobalEmulsion _ NULL!
*/
}
public static void exitTimeNonInherited() {
	TheGlobalEmulsion = null;
/*
Xanadu-Xpp-fluid.st:154:GlobalEmulsion class methodsFor: 'cleanup'!
exitTimeNonInherited
	TheGlobalEmulsion := NULL.!
*/
}
public static void linkTimeNonInherited() {
	TheGlobalEmulsion = null;
/*
Xanadu-Xpp-fluid.st:158:GlobalEmulsion class methodsFor: 'cleanup'!
linkTimeNonInherited
	TheGlobalEmulsion := NULL.!
*/
}
public GlobalEmulsion(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
