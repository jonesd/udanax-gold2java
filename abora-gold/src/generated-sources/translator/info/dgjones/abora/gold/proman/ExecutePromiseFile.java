/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.proman;

import info.dgjones.abora.gold.cobbler.Connection;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.XnReadFile;
import info.dgjones.abora.gold.java.missing.XnWriteFile;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeServer;
import info.dgjones.abora.gold.proman.ExecutePromiseFile;
import info.dgjones.abora.gold.proman.PairPortal;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.rcmain.ServerChunk;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xcvr.XnReadStream;
import info.dgjones.abora.gold.xcvr.XnWriteStream;

/**
 * Read client requests from one files and write the results to another file.
 */
public class ExecutePromiseFile extends ServerChunk {

	protected String myReadName;
	protected String myWriteName;
	protected PromiseManager myManager;
	protected Connection myConnection;
/*
udanax-top.st:50782:
ServerChunk subclass: #ExecutePromiseFile
	instanceVariableNames: '
		myReadName {char star}
		myWriteName {char star}
		myManager {PromiseManager NOCOPY}
		myConnection {Connection NOCOPY}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:50790:
ExecutePromiseFile comment:
'Read client requests from one files and write the results to another file.'!
*/
/*
udanax-top.st:50792:
(ExecutePromiseFile getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ExecutePromiseFile.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Execute the action defined by this thunk.
 */
public boolean execute() {
	myManager.handleRequest();
	Someone.thingToDo();
	/* Check whether the read file is Empty. */
	return true;
/*
udanax-top.st:50797:ExecutePromiseFile methodsFor: 'operate'!
{BooleanVar} execute
	"Execute the action defined by this thunk."
	myManager handleRequest.
	self thingToDo.  "Check whether the read file is Empty."
	^true!
*/
}
public void restartPromises(Rcvr rcvr) {
	XnReadStream readStream;
	XnWriteStream writeStream;
	readStream = XnReadFile.make(myReadName);
	writeStream = XnWriteFile.make(myWriteName);
	myManager = PromiseManager.make((PairPortal.make(readStream, writeStream)));
	myConnection = Connection.make(AboraSupport.findCategory(FeServer.class));
	Someone.thingToDo();
	/* This should be unnecessary. */
/*
udanax-top.st:50806:ExecutePromiseFile methodsFor: 'hooks:'!
{void RECEIVE.HOOK} restartPromises: rcvr {Rcvr unused}
	| readStream {XnReadStream} writeStream {XnWriteStream} |
	readStream _ XnReadFile make: myReadName.
	writeStream _ XnWriteFile make: myWriteName.
	myManager _ PromiseManager make: (PairPortal make: readStream with: writeStream).
	myConnection _ Connection make: FeServer.  self thingToDo.  "This should be unnecessary."!
*/
}
public ExecutePromiseFile(Rcvr receiver) {
	super(receiver);
	myReadName = receiver.receiveString();
	myWriteName = receiver.receiveString();
	restartPromises(receiver);
/*
udanax-top.st:50815:ExecutePromiseFile methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myReadName _ receiver receiveString.
	myWriteName _ receiver receiveString.
	self restartPromises: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendString(myReadName);
	xmtr.sendString(myWriteName);
/*
udanax-top.st:50821:ExecutePromiseFile methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendString: myReadName.
	xmtr sendString: myWriteName.!
*/
}
public ExecutePromiseFile() {
/*

Generated during transformation
*/
}
}
