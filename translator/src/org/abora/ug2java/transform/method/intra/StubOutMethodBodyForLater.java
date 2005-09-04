package org.abora.ug2java.transform.method.intra;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.MethodBody;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaComment;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.StringLiteral;
import org.abora.ug2java.transform.method.MethodTransformation;

public class StubOutMethodBodyForLater implements MethodTransformation {

	private static final Set METHODS;
	static {
		Set set = new HashSet();
		set.add("CoordinateSpace.verify");
		set.add("GenericCrossSpace.makeRcvr");
//		set.add("Recipe.staticTimeNonInherited");
		set.add("GrandHashTableTester.stomp");
		set.add("ActualCookbook.receiveClassList");
		set.add("ActualCookbook.sendClassList");
		set.add("GrandHashTableTester.runTest");
		set.add("FilterSpace.makeRcvr");
		set.add("Encrypter.invokeFunction");
		set.add("ExampleHIHHandler.handleRequest");
		set.add("Abraham.restartAbraham(Rcvr)");
		set.add("CBlockTrackingPacker.consistentCount");
		set.add("CBlockTrackingPacker.checkTracker");
		set.add("Mapping.make(Object,Object)");
		set.add("StackExaminer.pointersOnStack");
		set.add("StackExaminer.linkTimeNonInherited");
		set.add("FakeCategory.makeHooked");
		set.add("PromiseManager.sendIntegerVar");
		set.add("PromiseManager.makeFloatArray");
		set.add("PromiseManager.makeIntArray");
		set.add("PromiseManager.mapOverride");
		// May need to edition().newSharedWith() ?
		set.add("FeWorkSet.unionWith");
		set.add("FeWorkSet.works");
		set.add("FeWorkSet.make(PtrArray)");
		set.add("Binary2Rcvr.getIdentifier");
		//set.add("Recipe.Recipe");
		set.add("SpecialistRcvr.basicReceiveInto");
		set.add("TextyRcvr.receiveCategory");
		set.add("TextyXmtr.sendCategory");
		set.add("Category.inheritsFrom");
		set.add("Category.find");
		set.add("Category.create");
		set.add("Category.getCategory");
		set.add("Package.registerPackageCategory");
		set.add("Package.originalClass");
		set.add("Package.MAKEUHOOKED");
		set.add("Package.makeHooked");
		set.add("Package.Package(Heaper)");
		set.add("FakeCategory.withAllSuperclasses");
		set.add("FakePackageCategory.makePackage");
		set.add("FakePackageCategory.makeHooked");
		set.add("Emulsion.initImageEmulsions");
		set.add("FluidPromiseVar.FluidPromiseVar");
		set.add("FluidPromiseVar.fluidSet");
		set.add("FluidPromiseVar.make");
		set.add("FluidVar.make(String,BlockClosure,Emulsion,AboraClass)");
		set.add("FluidVar.cleanup");
		set.add("Emulsion.initialize");
		set.add("VolumeTester.allTestsOn");

		METHODS = Collections.unmodifiableSet(set);
	}

	public void transform(JavaMethod javaMethod) {
		String fullName = javaMethod.getQualifiedName();
		String signature = javaMethod.getQualifiedSignature();
		if (METHODS.contains(fullName) || METHODS.contains(signature)) {
			replaceBodyWithThrow(javaMethod);
		}
	}

	private void replaceBodyWithThrow(JavaMethod method) {
		List tokens = new ArrayList();
		tokens.add(new JavaComment("Transform: Convert code later"));
		tokens.add(new JavaKeyword("throw"));
		tokens.add(new JavaKeyword("new"));
		tokens.add(new JavaCallKeywordStart("UnsupportedOperationException"));
		tokens.add(new StringLiteral("Implement later"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());

		method.methodBody = new MethodBody(tokens);

	}
}