/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaBlockEnd;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaComment;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class ChooseTransformOnly extends AbstractMethodBodyTransformation {

	private static final List TRANSLATE_METHODS;
	static {
		List list = new ArrayList();
		list.add("IntegerPos.actualHashForEqual");
		list.add("IntegerPos.integerHash");
		// Remove smalltalkOnly logging
		list.add("WorksIniter.initializeSystem");
		list.add("PrintCBlocksTracks.execute");
		list.add("PrimFloatSpec");
		list.add("PrimPointerSpec");
		list.add("HUpperCrum.addOParent");
		list.add("AgendaItem.schedule");
		list.add("CloseExecutor.registerHolder");
		list.add("RealPos.actualHashForEqual");
		list.add("PrimIntegerSpec.array");
		list.add("PrimIntegerSpec.arrayFromBuffer");
		list.add("PrimIntegerSpec.privateCopy");
		list.add("PrimIEEE64.asIEEE32");
		list.add("PrimIEEE32.asIEEE64");
		list.add("ActualHashSet.distanceFromHome");
		//TODO actually would like the debug info...
		list.add("TestPacker.mustBeInsideTransaction");
		list.add("IEEE32Pos.asIEEE");
		list.add("IEEE32Pos.asIEEE64");
		list.add("PromiseManager.make");
		//char vs byte problems in this whole area
		list.add("ReadArrayStream.next");
		list.add("ReadMemStream.next");
		
		//list.add("Abraham.becomeStub");
//		list.add("Abraham.dismantle");
//		list.add("Abraham.destroy");
//		list.add("Abraham.flockInfo");
//		list.add("Abraham.getInfo");
//		list.add("Abraham.token");
//		list.add("Abraham.Abraham");
//		list.add("Abraham.restartAbraham");
//		list.add("Abraham.initTimeNonInherited");
		
		//TODO might need for debugging...
		list.add("TokenSource.takeToken");
		
		list.add("DiskManager.emulsion");
		list.add("FlockInfo.FlockInfo");
		list.add("FlockInfo.token");
		list.add("FlockInfo.fetchShepherd");
		list.add("FlockInfo.registerInfo");
		
		list.add("Binary2Xmtr.linkTimeNonInherited");
		// Duplicate for all & smalltalk
		list.add("GrandEntry.GrandEntry");
		
		list.add("ServerChunk.emulsion");
		
		list.add("TextyXmtr.sendCategory");
		list.add("TextyRcvr.receiveCategory");
		list.add("TextyRcvr.getIdentifier");
		
		list.add("ReadMemStream.getByte");
		list.add("ReadMemStream.putBack");

		//TODO only for tests...
		list.add("translateOnly");

		TRANSLATE_METHODS = Collections.unmodifiableList(list);
	}

	private static final List SMALLTALK_METHODS;
	static {
		List list = new ArrayList();
		list.add("WorksIniter.fetchNewRawSpace");
		list.add("DiskManagerEmulsion.fetchNewRawSpace");
		list.add("BeGrandMap.xuTime");
		//TODO not sure about this choice
		list.add("Abraham.getShepherdStubCategory");
		list.add("DeleteExecutor.registerHolder");
		list.add("IPPromiseListener.shouldBeReady");
		list.add("SHTO.printOn");
		list.add("ShepherdLocked.isReallyUnlocked");
		list.add("CloseExecutor.execute");
		list.add("PrimIEEE32.actualHashForEqual");
		list.add("PrimIEEE64.actualHashForEqual");
		list.add("PrimIEEE32.exponent");
		list.add("PrimIEEE64.exponent");
		list.add("PrimIntegerSpec.canHold");
		list.add("TextyRcvr.receiveString");
		list.add("Binary2Rcvr.receiveString");
		list.add("WriteMemStream.contents");
		list.add("PromiseManager.respondBooleanVar");
		//list.add("PrimIntegerSpec.PrimIntegerSpec");
		list.add("ExponentialHashMap.initTimeNonInherited");
		list.add("GlobalEmulsion.fetchNewRawSpace");
		list.add("Ent.Ent");
		list.add("CategoryRecipe.staticTimeNonInherited");
		list.add("FlockInfo.staticTimeNonInherited");
		list.add("TextyRcvr.linkTimeNonInherited");
		list.add("FDListener.initTimeNonInherited");
		list.add("HUpperCrum.propagateBCrum");
		list.add("SimpleShuffler.shuffle16");
		list.add("SimpleShuffler.shuffle32");
		list.add("WorksWaitDetector.WorksWaitDetector");
		list.add("WorksWaitDetector.done");
		list.add("SnarfPacker.makePersistent");
		list.add("SnarfHandler.SnarfHandler");
		list.add("TextyXmtr.sendString");
		list.add("Recipe.staticTimeNonInherited");
		list.add("WriteMemStream.putStr");
		list.add("WriteArrayStream.putStr");
		list.add("DiskCountSpecialist.sendHeaperTo");
		list.add("DiskSpecialist.sendHeaperTo");
		list.add("SnarfPacker.purgeClean");
		list.add("TestPacker.purgeClean");
		list.add("SensorCrum.printOn");
		list.add("SplitLoaf.printOn");
		
		list.add("TextyXmtr.sendIEEEDoubleVar");
		list.add("TextyXmtr.sendInt32");
		list.add("TextyXmtr.sendInt8");
		list.add("TextyXmtr.sendIntegerVar");
		list.add("TextyXmtr.sendUInt32");
		list.add("TextyXmtr.sendUInt8");

		
		//TODO debug/halt messages
		list.add("SnarfRecord.dismantleFlock");
		
		//TODO just switched on for extra logging. Wrong?
		list.add("ActualHashSet.ActualHashSet");
		list.add("ActualHashSet.store");
		list.add("ActualHashSet.linkTimeNonInherited");
		list.add("ActualHashSet.stepper");
		list.add("ActualHashSet.hasMember");
		list.add("ActualHashSet.wipeAll");
		list.add("ActualHashSet.storeAll");
		list.add("ActualHashSet.printInternals");
		list.add("ActualHashSet.introduce");
		list.add("ActualHashSet.wipe");
		list.add("ActualHashSet.remove");

		list.add("Abraham.becomeStub");
		list.add("Abraham.dismantle");
		list.add("Abraham.destroy");
		list.add("Abraham.flockInfo");
		list.add("Abraham.getInfo");
		list.add("Abraham.token");
		list.add("Abraham.Abraham");
		list.add("Abraham.restartAbraham");
		list.add("Abraham.initTimeNonInherited");
		
		//TODO only for tests...
		list.add("smalltalkOnly");

		SMALLTALK_METHODS = Collections.unmodifiableList(list);
	}

	private static final List NEITHER_METHODS;
	static {
		List list = new ArrayList();
		list.add("ExponentialHashMap.linkTimeNonInherited");
		list.add("PrimIntegerSpec.PrimIntegerSpec");
		
		//TODO should choose transform/smalltalk for these - missing out on debug info without them
		list.add("RegionTester.testUnaryRegionOpsOn");
		list.add("RegionTester.testBinaryRegionOpsOn");

		NEITHER_METHODS = Collections.unmodifiableList(list);
	}

	public ChooseTransformOnly() {
		super();
	}
	public ChooseTransformOnly(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaBlockEnd.class),
				factory.any(
						factory.token(JavaIdentifier.class, "translateOnly"),
						factory.token(JavaIdentifier.class, "smalltalkOnly")),
				factory.token(JavaStatementTerminator.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier onlyType = (JavaIdentifier)tokens.get(i+1);
		String call = onlyType.value;
		boolean isTranslateOnly = call.equals("translateOnly");
		
		String shortName = javaMethod.name;
		String className = javaMethod.javaClass.className;
		String fullName = className+"."+shortName;
		
		boolean shouldTranslate = TRANSLATE_METHODS.contains(shortName) || TRANSLATE_METHODS.contains(className) || TRANSLATE_METHODS.contains(fullName);
		boolean shouldSmalltalk = SMALLTALK_METHODS.contains(shortName) || SMALLTALK_METHODS.contains(className) ||SMALLTALK_METHODS.contains(fullName);
		boolean shouldDelete = NEITHER_METHODS.contains(shortName) || NEITHER_METHODS.contains(className) ||NEITHER_METHODS.contains(fullName);
		
		if (isTranslateOnly) {
			if (shouldTranslate) {
				return acceptOnlyBlock(javaMethod, tokens, i);
			} else if (shouldSmalltalk || shouldDelete) {
				return rejectOnlyBlock(javaMethod, tokens, i, call);
			}
		} else {
			if (shouldTranslate || shouldDelete) {
				return rejectOnlyBlock(javaMethod, tokens, i, call);
			} else if (shouldSmalltalk) {
				return acceptOnlyBlock(javaMethod, tokens, i);
			}
		}
		
		return simpleBlock(javaMethod, tokens, i, call);
	}
		
	private int acceptOnlyBlock(JavaMethod javaMethod, List tokens, int i) {
		int blockStart = javaMethod.methodBody.findStartOfBlock(i);
		tokens.remove(i+2);
		tokens.remove(i+1);
		tokens.remove(i);
		tokens.remove(blockStart);
		
		return i - 1;
	}

	private int rejectOnlyBlock(JavaMethod javaMethod, List tokens, int i, String call) {
		int blockStart = javaMethod.methodBody.findStartOfBlock(i);
		for (int j = i+2; j >= blockStart; --j) {
			tokens.remove(j);
		}
		tokens.add(blockStart, new JavaComment("Removed "+call));
		return blockStart;
	}

	private int simpleBlock(JavaMethod javaMethod, List tokens, int i, String call) {
		int blockStart = javaMethod.methodBody.findStartOfBlock(i);
		tokens.remove(i+2);
		tokens.remove(i+1);

		tokens.add(blockStart, new JavaIdentifier("AboraSupport"));
		tokens.add(blockStart+1, new JavaCallStart(call));
		tokens.add(blockStart+2, new JavaCallEnd());
		tokens.add(blockStart+3, new JavaStatementTerminator());
				
		return i;
	}
}
