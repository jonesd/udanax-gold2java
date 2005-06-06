/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java;

import java.io.PrintWriter;

import org.abora.gold.java.missing.OrderEnum;
import org.abora.gold.rcmain.ServerChunk;
import org.abora.gold.spaces.integers.IntegerPos;
import org.abora.gold.xcvr.Rcvr;
import org.abora.gold.xcvr.Recipe;
import org.abora.gold.xpp.basic.Category;
import org.abora.gold.xpp.fluid.FluidVar;

public class AboraHeaper {
	public static FluidVar ActiveClubs;
	public static FluidVar CurrentAuthor;
	public static FluidVar CurrentBertCanopyCache;
	public static FluidVar CurrentBertCrum;
	public static FluidVar CurrentGrandMap;
	public static FluidVar CurrentKeyMaster;
	public static FluidVar CurrentPacker;
	public static FluidVar CurrentSensorCanopyCache;
	public static FluidVar CurrentServer;
	public static FluidVar CurrentServerConnection;
	public static FluidVar CurrentServerLoop;
	public static FluidVar CurrentSession;
	public static FluidVar CurrentSessions;
	public static FluidVar CurrentTrace;
	public static FluidVar GrandConnection;
	public static FluidVar InitialEditClub;
	public static FluidVar InitialOwner;
	public static FluidVar InitialReadClub;
	public static FluidVar InitialSponsor;
	public static FluidVar InsideTransactionFlag;
	public static FluidVar InsideAgenda;
	
	public static Recipe BootCuisine;
	public static Recipe CalcCuisine;
	public static Recipe DiskCuisine;
	public static Recipe FebeCuisine;
	public static Recipe XppCuisine;
	
	public static ServerChunk CurrentChunk;

	//TODO what to really do with these, often Smalltalk Symbols?
	public static final String ALL_TESTS_ON_ = "AllTestsOn";
	public static final String BLAST_LOG = "BLAST_LOG";
	public static final String BOOT_CUISINE = "BootCuisine";
	public static final String CALC_CUISINE = "CalcCuisine";
	public static final String CHECK_ = "Check";
	public static final String CONCRETE = "Concrete";
	public static final String CURRENT_CHUNK = "CurrentChunk";
	public static final String DISK_CUISINE = "DiskCuisine";
	public static final String DONT_CHANGE_TURTLES_BOOT_HEAPER = "DontChangeTurtlesBootHeaper";
	public static final String EDITION_UWITH_ALL_UN3_WITH_WITH_ = "EDITION_UWITH_ALL_UN3_WITH_WITH_";
	public static final String FEBE_CUISINE = "FEBE_CUISINE";
	public static final String FOO_LOG = "FOO_LOG";
	public static final String MAKE_WRAPPER_ = "MakeWrapper";
	public static final String NO_ENCRYPTER = "NO_ENCRYPTER";
	public static final String NO_REQUEST_ = "NO_REQUEST_";
	public static final String PACKAGE = "PACKAGE";
	public static final String PACKAGEHOOK = "PACKAGEHOOK";
	public static final String SET_SPEC_ = "SetSpec";
	public static final String XPP_CUISINE = "XPP_CUISINE";
	
	public static final String FE_BOO_LOCK_SMITH = "FE_BOO_LOCK_SMITH";
	public static final String FE_CHALLENGE_LOCK_SMITH = "FE_CHALLENGE_LOCK_SMITH";
	public static final String FE_CLUB_DESCRIPTION = "FE_CLUB_DESCRIPTION";
	public static final String FE_HYPER_LINK = "FE_HYPER_LINK";
	public static final String FE_HYPER_REF = "FE_HYPER_REF";
	public static final String FE_LOCK_SMITH = "FE_LOCK_SMITH";
	public static final String FE_MATCH_LOCK_SMITH = "FE_MATCH_LOCK_SMITH";
	public static final String FE_MULTI_LOCK_SMITH = "FE_MULTI_LOCK_SMITH";
	public static final String FE_MULTI_REF = "FE_MULTI_REF";
	public static final String FE_PATH = "FE_PATH";
	public static final String FE_SET = "FE_SET";
	public static final String FE_SINGLE_REF = "FE_SINGLE_REF";
	public static final String FE_TEXT = "FE_TEXT";
	public static final String FE_WALL_LOCK_SMITH = "FE_WALL_LOCK_SMITH";
	public static final String FE_WORK_SET = "FE_WORK_SET";
	public static final String FE_WRAPPER = "FE_WRAPPER";
	
	public static final OrderEnum EQUAL = null;
	public static final OrderEnum GREATERUTHAN = null;
	public static final OrderEnum LESSUTHAN = null;
	public static final OrderEnum INCOMPARABLE = null;
	
	public static final IntegerPos ZERO = null;
	public static final IntegerPos ONE = null;
	public static final IntegerPos TWO = null;
	public static final IntegerPos THREE = null;
	
	public static final PrintWriter BlastLog = null;
	public static final PrintWriter ErrorLog = null;
	public static final PrintWriter FooLog = null;
	public static final PrintWriter VanillaLog = null;

	/**
	 * Constructor for AboraHeaper.
	 */
	public AboraHeaper() {
		super();
	}
	public AboraHeaper(Rcvr receiver) {
		throw new UnsupportedOperationException();
	}

	public static void knownBug() {
		throw new UnsupportedOperationException();
	}
	public static void thingToDo() {
		throw new UnsupportedOperationException();
	}
	public static void shouldImplement() {
		throw new UnsupportedOperationException();
	}
	public static void shouldNotImplement() {
		throw new UnsupportedOperationException();
	}
	public static void error(String message) {
		throw new UnsupportedOperationException();
	}
	public static void hack() {
		throw new UnsupportedOperationException();
	}
	public static void willNotImplement() {
		throw new UnsupportedOperationException();
	}
	public static void mightNotImplement() {
		throw new UnsupportedOperationException();
	}
	public static void REQUIRES(Category cat) {
		throw new UnsupportedOperationException();
	}
	public void stubbleForSubclassResponsibility() {
		throw new UnsupportedOperationException();
	}
	public int asOop() {
		throw new UnsupportedOperationException();
	}
	public void halt() {
		throw new UnsupportedOperationException();
	}
	public Object inspect() {
		throw new UnsupportedOperationException();
	}
	public void basicInspect() {
		throw new UnsupportedOperationException();
	}
	public String displayString() {
		throw new UnsupportedOperationException();
	}
	
	// Temp replace Heaper implementation
	public static int takeOop() {
		throw new UnsupportedOperationException();
	}
	
	// Temp replace Heaper implementation
	public static Category getCategory() {
		throw new UnsupportedOperationException();
	}
	
	// Temp replace Heaper implementation
	public void delete() {
		throw new UnsupportedOperationException();
	}

	// Temp replace Heaper implementation
	public void destructor() {
		throw new UnsupportedOperationException();
	}
	
	// Temp replace Heaper implementation
	public static boolean isDestructed(AboraHeaper h) {
		throw new UnsupportedOperationException();
	}
	public static boolean isConstructed(AboraHeaper h) {
		throw new UnsupportedOperationException();
	}
	public static void setGC(boolean a) {
		throw new UnsupportedOperationException();
	}
	public String printString() {
		throw new UnsupportedOperationException();
	}
	public void markInstances(int i) {
		throw new UnsupportedOperationException();
	}
	public static void mayBecome(Category cat) {
		throw new UnsupportedOperationException();
	}
	
	public boolean isEqualOrSubclassOf(Category cat) {
		//TODO should be added to Category?
		throw new UnsupportedOperationException();
	}
	public boolean isKindOf(Category cat) {
		throw new UnsupportedOperationException();
	}
	public static int preorderMax() {
		throw new UnsupportedOperationException();
	}
	public void fixup() {
		throw new UnsupportedOperationException();
	}
	public static Fn pointerToStaticMember(String name, String handlerType) {
		throw new UnsupportedOperationException();
	}
}
