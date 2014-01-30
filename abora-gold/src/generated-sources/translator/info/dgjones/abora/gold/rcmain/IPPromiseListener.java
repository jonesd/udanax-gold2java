/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.rcmain;

import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.SocketPortal;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nadmin.FePromiseSession;
import info.dgjones.abora.gold.nadmin.FeSession;
import info.dgjones.abora.gold.proman.PacketPortal;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.rcmain.FDListener;
import info.dgjones.abora.gold.rcmain.IPPromiseListener;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.XnBufferedReadStream;

/**
 * A IPConnectionListener is associated with the FD of a socket connection to a frontend.
 * Its handleInput method is used to invoke a waitForAndProcessMessage method to handle
 * messages
 * from the frontend.
 */
public class IPPromiseListener extends FDListener {

	protected PromiseManager myManager;
	protected FeSession mySession;
	protected PacketPortal myPortal;
/*
udanax-top.st:50900:
FDListener subclass: #IPPromiseListener
	instanceVariableNames: '
		myManager {PromiseManager}
		mySession {FeSession}
		myPortal {PacketPortal}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-rcmain'!
*/
/*
udanax-top.st:50907:
IPPromiseListener comment:
'A IPConnectionListener is associated with the FD of a socket connection to a frontend.
Its handleInput method is used to invoke a waitForAndProcessMessage method to handle messages
from the frontend.'!
*/
/*
udanax-top.st:50911:
(IPPromiseListener getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:50982:
IPPromiseListener class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:50985:
(IPPromiseListener getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(IPPromiseListener.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public void destruct() {
	mySession = null;
	myPortal = null;
	myManager.destroy();
	myManager = null;
	super.destruct();
/*
udanax-top.st:50916:IPPromiseListener methodsFor: 'protected: destruct'!
{void} destruct
	mySession _ NULL.
	myPortal _ NULL.
	myManager destroy.
	myManager _ NULL.
	super destruct!
*/
}
public IPPromiseListener(int aSocket) {
	super();
	CurrentChunk = this;
	myPortal = SocketPortal.make(aSocket);
	myManager = PromiseManager.make(myPortal);
	registerFor(aSocket);
	FePromiseSession.make((UInt8Array.string("socket")), this, myManager);
	CurrentChunk = null;
/*
udanax-top.st:50925:IPPromiseListener methodsFor: 'creation'!
create: aSocket {int}
	super create.
	CurrentChunk _ self.
	myPortal _ SocketPortal make: aSocket.
	myManager _ PromiseManager make: myPortal.
	self registerFor: aSocket.
	FePromiseSession make: (UInt8Array string: 'socket') with: self with: myManager.
	CurrentChunk _ NULL.!
*/
}
public boolean shouldBeReady() {
	boolean result;
	result = ! destroyPending();
	if ( ! (result)) {
		destroyOKIfRequested();
	}
	return result;
/*
udanax-top.st:50937:IPPromiseListener methodsFor: 'testing'!
{BooleanVar} shouldBeReady
	| result {BooleanVar} |
	result := self destroyPending not.
	result ifFalse: [self destroyOKIfRequested].
	[^result] smalltalkOnly.
	'
#if defined(WIN32) | defined(HIGHC)
	return result;
#else
	if (!!result) {
		return FALSE;
	}
	size_t	nready;
	ioctl (this->descriptor (), FIONREAD, &nready);
	return nready > 0;
#endif /- WIN32 -/
' translateOnly!
*/
}
/**
 * Attempt to execute another chunk.  Return whether there is more to be done.
 */
public boolean execute() {
	if ( ! (((XnBufferedReadStream) myPortal.readStream()).isReady())) {
		return false;
	}
	CurrentChunk = this;
	try {
		inRequest();
		myManager.handleRequest();
		notInRequest();
	}
	catch (AboraRuntimeException ex) {
		if (AboraRuntimeException.SOCKETURECVUERROR.equals(ex.getMessage()) || AboraRuntimeException.SOCKETUSENDUERROR.equals(ex.getMessage())) {
			AboraSupport.translateOnly();
			{
				// /*cerr << &PROBLEM(ex);*/
			}
			AboraSupport.translateOnly();
			{
				/* operator<<(cerr ,(Problem*)&PROBLEM(ex)); */
			}
			AboraSupport.logger.print(" Connection closed.\n"+
"");
			CurrentChunk = null;
			destroy();
			destroyOKIfRequested();
			return false;
		}
		else {
			throw ex;
		}
	}
	CurrentChunk = null;
	if (destroyOKIfRequested()) {
		return false;
	}
	else {
		return ((XnBufferedReadStream) myPortal.readStream()).isReady();
	}
/*
udanax-top.st:50957:IPPromiseListener methodsFor: 'accessing'!
{BooleanVar} execute
	"Attempt to execute another chunk.  Return whether there is more to be done."
	(myPortal readStream cast: XnBufferedReadStream) isReady ifFalse: [^false].
	CurrentChunk _ self.
	FDListener problems.SOCKET.U.ERRS 
		handle: [:ex | 
				'/-cerr << &PROBLEM(ex);-/' translateOnly.
				'operator<<(cerr ,(Problem*)&PROBLEM(ex));' translateOnly.
				cerr << ' Connection closed.
'.
				CurrentChunk _ NULL.
				self destroy.
				self destroyOKIfRequested.
				^false]
		do: 	[
			self inRequest.
			myManager handleRequest.
			self notInRequest].
	CurrentChunk _ NULL.
	self destroyOKIfRequested
		ifTrue: [^ false]
		ifFalse: [^(myPortal readStream cast: XnBufferedReadStream) isReady]!
*/
}
public static FDListener make(int aSocket) {
	return new IPPromiseListener(aSocket);
/*
udanax-top.st:50990:IPPromiseListener class methodsFor: 'creation'!
{FDListener} make: aSocket {int}
	^self create: aSocket.!
*/
}
public IPPromiseListener() {
/*

Generated during transformation
*/
}
public IPPromiseListener(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
