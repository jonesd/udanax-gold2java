/**
 * The MIT License
 * Copyright (c) 2003 David G Jones
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package info.dgjones.abora.ug2java.transform.method.intra;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.transform.method.MethodTransformation;

public class ExcludeMethodsByName implements MethodTransformation {

	private static final List INSTANCE_METHODS;
	static {
		List list = new ArrayList();
		list.add("ActualArray.search"); // seemes to be smalltalk only code - not int aware
		list.add("XnRegion.dox"); // smalltalk: special
		list.add("ScruSet.dox"); // smalltalk: special
		list.add("ScruTable.dox"); // smalltalk: special
		list.add("ScruTable.asOrderedCollection"); // smalltalk: special
		list.add("IntegerPos.basicCast");
		list.add("MuTable.test");
		//TODO Possibly dropped for myFlags?
		list.add("CanopyCrum.joint");
		//TODO would like this for debugging, cant compile all cases though...
		list.add("crums");
		list.add("Abraham.isKindOf");
		list.add("BranchDescription.ohashForEqual");
		list.add("BranchDescription.oisEqual");
		list.add("Stepper.forEach");
		list.add("Stepper.forEachPromise");
		list.add("TableStepper.forIndices");
		list.add("TableStepper.forKeyValues");
		list.add("TableStepper.forPositions");
		list.add("TableStepper.forPromisedPairs");
		list.add("RecorderFossil.reanimate");
		list.add("FullPropChange.joinProp"); // suspended
		list.add("Binary2Rcvr.uint8"); // TODO Could be an earlier parsing problem behind why we have these?
		list.add("Binary2Xmtr.uint8"); // TODO Could be an earlier parsing problem behind why we have these?
		list.add("DiskManager.consistent");
		list.add("XnRegion.mapping");
		list.add("Xmtr.send(Object)");
		list.add("FluidPromiseVar.Emulsion");
		list.add("FluidVar.fluidBindDuring");
		list.add("FeServer.newClubDescription");
		
		list.add("Category.getCategory");

		INSTANCE_METHODS = Collections.unmodifiableList(list);
	}

	private static final List STATIC_METHODS;
	static {
		List list = new ArrayList();
		list.add("ImmuSet.with"); 
		list.add("IntegerPos.IntegerVar");
		list.add("ImmuSet.create");
		list.add("ImmuSet.make(Object)");
		list.add("MuSet.make(Object)");
		list.add("ArrayAccumulator.create");
		list.add("OffsetArrayStepper.create");
		list.add("OffsetScruTableStepper.create");
		list.add("IntegerTableStepper.createWithOrderSpec");
		list.add("MuTable.test");
		list.add("IntegerTable.make(Heaper)");
		list.add("IntegerTableStepper.create(IntegerTable,OrderSpec)");
		list.add("Encrypter.make(Sequence,Sequence)");
		list.add("DiskManager.consistent");
		list.add("DiskManager.insistent");
		list.add("PropChanger.make(CanopyCrum,PropChange)");
		//Seems to be incorrect default parameter code
		list.add("CrossOrderSpec.make(Object)");
		//Seems to be incorrect default parameter code
		list.add("CrossOrderSpec.make(Object,Object)");
		list.add("Tester.defaultRcString");
		list.add("Tester.publicClass");

		STATIC_METHODS = Collections.unmodifiableList(list);
	}

	
	public void transform(JavaMethod javaMethod) {
		String shortName = javaMethod.getName();
		String fullName = javaMethod.getQualifiedName();
		String parameterName = javaMethod.getQualifiedSignature();
		if (!javaMethod.isStatic() && (INSTANCE_METHODS.contains(shortName) || INSTANCE_METHODS.contains(fullName) || INSTANCE_METHODS.contains(parameterName))
				|| javaMethod.isStatic() && (STATIC_METHODS.contains(shortName) || STATIC_METHODS.contains(fullName) || STATIC_METHODS.contains(parameterName))
				//TODO include inspect in lookup mechanism
				|| javaMethod.name.startsWith("inspect")) {
			javaMethod.shouldInclude = false;
		}
	}
	

}