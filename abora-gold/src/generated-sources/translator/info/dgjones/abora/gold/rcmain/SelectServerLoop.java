/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.rcmain;

import info.dgjones.abora.gold.fm.support.Thunk;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Delay;
import info.dgjones.abora.gold.java.missing.smalltalk.Processor;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.rcmain.SelectServerLoop;
import info.dgjones.abora.gold.rcmain.ServerChunk;
import info.dgjones.abora.gold.rcmain.ServerLoop;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * This is a ServerLoop designed specifically for Berkeley Sockets.  It allows socket
 * listeners to be registered and it dispatches among them based on a select() call
 */
public class SelectServerLoop extends ServerLoop {

	protected int myFDSet;
/*
udanax-top.st:57631:
ServerLoop subclass: #SelectServerLoop
	instanceVariableNames: 'myFDSet {fd.U.set var NOCOPY}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-rcmain'!
*/
/*
udanax-top.st:57635:
SelectServerLoop comment:
'This is a ServerLoop designed specifically for Berkeley Sockets.  It allows socket listeners to be registered and it dispatches among them based on a select() call'!
*/
/*
udanax-top.st:57637:
(SelectServerLoop getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:57737:
SelectServerLoop class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:57740:
(SelectServerLoop getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SelectServerLoop.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}).add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public SelectServerLoop() {
	super();
	AboraSupport.translateOnly();
	{
		/* 
#ifndef HIGHC
	FD_ZERO(&myFDSet);
#endif */
	}
/*
udanax-top.st:57642:SelectServerLoop methodsFor: 'creation'!
create
	super create.
	'
#ifndef HIGHC
	FD_ZERO(&myFDSet);
#endif' translateOnly!
*/
}
/**
 * Schedule any chunks that have become active.
 */
public void scheduleChunks() {
	AboraSupport.smalltalkOnly();
	{
		Delay.forMilliseconds(100);
	}
	AboraSupport.translateOnly();
	{
		// 
// #ifdef unix
// 	signal (SIGPIPE, SIG_IGN);
// #endif
// 	
// 	static fd_set readfds = myFDSet;
// 	static fd_set exceptfds = myFDSet;
// #if defined(unix) && ! defined(__sgi)
// 	int maxFDs = getdtablesize();
// #else
// 	int maxFDs = FD_SETSIZE;
// #endif /* unix */
// 	int numReady;
// 	
// 	if (this->activeChunks ()->isEmpty ()) {
// 		numReady = select (maxFDs-1, &readfds, NULL, &exceptfds, NULL);
// 	} else {
// 		/* timeout immediately so active Chunks can execute */
// 		timeval zero;
// 		zero.tv_sec = 0;
// 		zero.tv_usec = 0;
// 		numReady = select (maxFDs-1, &readfds, NULL, &exceptfds, &zero);
// 	}
// 	if (numReady <= 0 ) {
// #ifdef WIN32
// 		if (numReady == 0 || errno == WSAEINTR) {
// #else
// 		if (numReady == 0 || errno == EINTR) {
// #endif /* WIN32 */
// 			return;
// 		}
// 		BLAST(SELECT_FAILED);
// 	}
// 	BEGIN_FOR_EACH(FDListener, aListener, (ServerLoop::chunks()->stepper())) {
// 		if (FD_ISSET(aListener->descriptor(), &exceptfds)) {
// 			ServerLoop::chunks()->remove(aListener);
// 			aListener->destroy ();
// 			if (--numReady <= 0)
// 				break;
// 		} else if (FD_ISSET(aListener->descriptor(), &readfds)) {
// 			if (aListener->shouldBeReady ()) {
// 				this->activeChunks()->store(aListener);
// 			} else {
// 				ServerLoop::chunks()->remove(aListener);
// 				aListener->destroy ();
// 			}
// 			if (--numReady <= 0)
// 				break;
// 		}
// 	} END_FOR_EACH;
// 
	}
	AboraSupport.smalltalkOnly();
	{
		activeChunks().storeAll(ServerLoop.chunks());
		Processor.yield();
	}
/*
udanax-top.st:57651:SelectServerLoop methodsFor: 'execution'!
{void} scheduleChunks
	"Schedule any chunks that have become active."
	
	[Stepper] USES.
	[MuSet] USES.
	[Delay forMilliseconds: 100.] smalltalkOnly.
	'
#ifdef unix
	signal (SIGPIPE, SIG_IGN);
#endif
	
	static fd_set readfds = myFDSet;
	static fd_set exceptfds = myFDSet;
#if defined(unix) && !! defined(__sgi)
	int maxFDs = getdtablesize();
#else
	int maxFDs = FD_SETSIZE;
#endif /- unix -/
	int numReady;
	
	if (this->activeChunks ()->isEmpty ()) {
		numReady = select (maxFDs-1, &readfds, NULL, &exceptfds, NULL);
	} else {
		/- timeout immediately so active Chunks can execute -/
		timeval zero;
		zero.tv_sec = 0;
		zero.tv_usec = 0;
		numReady = select (maxFDs-1, &readfds, NULL, &exceptfds, &zero);
	}
	if (numReady <= 0 ) {
#ifdef WIN32
		if (numReady == 0 || errno == WSAEINTR) {
#else
		if (numReady == 0 || errno == EINTR) {
#endif /- WIN32 -/
			return;
		}
		BLAST(SELECT_FAILED);
	}
	BEGIN_FOR_EACH(FDListener, aListener, (ServerLoop::chunks()->stepper())) {
		if (FD_ISSET(aListener->descriptor(), &exceptfds)) {
			ServerLoop::chunks()->remove(aListener);
			aListener->destroy ();
			if (--numReady <= 0)
				break;
		} else if (FD_ISSET(aListener->descriptor(), &readfds)) {
			if (aListener->shouldBeReady ()) {
				this->activeChunks()->store(aListener);
			} else {
				ServerLoop::chunks()->remove(aListener);
				aListener->destroy ();
			}
			if (--numReady <= 0)
				break;
		}
	} END_FOR_EACH;
' translateOnly.
	[self activeChunks storeAll: ServerLoop chunks.
	Processor yield] smalltalkOnly!
*/
}
public void deregisterChunk(ServerChunk aChunk) {
	super.deregisterChunk(aChunk);
	AboraSupport.translateOnly();
	{
		/* 
	FD_CLR(CAST(FDListener,aChunk)->descriptor(), &myFDSet);
 */
	}
/*
udanax-top.st:57714:SelectServerLoop methodsFor: 'protected: accessing'!
{void} deregisterChunk: aChunk {ServerChunk}
	super deregisterChunk: aChunk.
	'
	FD_CLR(CAST(FDListener,aChunk)->descriptor(), &myFDSet);
' translateOnly!
*/
}
public void registerChunk(ServerChunk aChunk) {
	super.registerChunk(aChunk);
	AboraSupport.translateOnly();
	{
		/* 
	FD_SET(CAST(FDListener,aChunk)->descriptor(), &myFDSet);
 */
	}
/*
udanax-top.st:57721:SelectServerLoop methodsFor: 'protected: accessing'!
{void} registerChunk: aChunk {ServerChunk}
	super registerChunk: aChunk.
	'
	FD_SET(CAST(FDListener,aChunk)->descriptor(), &myFDSet);
' translateOnly!
*/
}
public SelectServerLoop(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:57730:SelectServerLoop methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:57733:SelectServerLoop methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public static Thunk make() {
	return new SelectServerLoop();
/*
udanax-top.st:57745:SelectServerLoop class methodsFor: 'creation'!
{Thunk} make
	^ self create!
*/
}
}
