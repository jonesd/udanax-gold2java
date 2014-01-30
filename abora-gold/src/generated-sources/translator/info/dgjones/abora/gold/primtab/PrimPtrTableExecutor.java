/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.primtab;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.primtab.PrimPtrTable;
import info.dgjones.abora.gold.primtab.PrimPtrTableExecutor;
import info.dgjones.abora.gold.wparray.XnExecutor;
import info.dgjones.abora.gold.xcvr.Rcvr;

public class PrimPtrTableExecutor extends XnExecutor {

	protected PrimPtrTable myTable;
	protected XnExecutor myFollower;
/*
udanax-top.st:33784:
XnExecutor subclass: #PrimPtrTableExecutor
	instanceVariableNames: '
		myTable {PrimPtrTable}
		myFollower {XnExecutor | NULL}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-primtab'!
*/
/*
udanax-top.st:33790:
(PrimPtrTableExecutor getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:33806:
PrimPtrTableExecutor class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:33809:
(PrimPtrTableExecutor getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PrimPtrTableExecutor.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public void execute(int estateIndex) {
	myTable.weakRemove(estateIndex, myFollower);
/*
udanax-top.st:33795:PrimPtrTableExecutor methodsFor: 'invoking'!
{void} execute: estateIndex {Int32}
	myTable weakRemove: estateIndex with: myFollower!
*/
}
public PrimPtrTableExecutor(PrimPtrTable table, XnExecutor follower) {
	super();
	myTable = table;
	myFollower = follower;
/*
udanax-top.st:33800:PrimPtrTableExecutor methodsFor: 'protected: create'!
create: table {PrimPtrTable} with: follower {XnExecutor | NULL}
	super create.
	myTable := table.
	myFollower := follower.!
*/
}
public static PrimPtrTableExecutor make(PrimPtrTable table, XnExecutor follower) {
	return new PrimPtrTableExecutor(table, follower);
/*
udanax-top.st:33814:PrimPtrTableExecutor class methodsFor: 'create'!
make: table {PrimPtrTable} with: follower {XnExecutor | NULL}
	^ self create: table with: follower!
*/
}
public PrimPtrTableExecutor() {
/*

Generated during transformation
*/
}
public PrimPtrTableExecutor(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
