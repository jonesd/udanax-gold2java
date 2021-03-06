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
package info.dgjones.abora.gold.java;

import java.io.File;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import info.dgjones.abora.gold.appmods.WorksIniter;
import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.be.canopy.CanopyCache;
import info.dgjones.abora.gold.cobbler.BootMaker;
import info.dgjones.abora.gold.cobbler.Connection;
import info.dgjones.abora.gold.collection.basic.IEEE32Array;
import info.dgjones.abora.gold.collection.basic.IEEE64Array;
import info.dgjones.abora.gold.collection.basic.Int32Array;
import info.dgjones.abora.gold.collection.basic.Int8Array;
import info.dgjones.abora.gold.collection.basic.IntegerVarArray;
import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.collection.basic.PrimDataArray;
import info.dgjones.abora.gold.collection.basic.PrimFloatArray;
import info.dgjones.abora.gold.collection.basic.PrimIntArray;
import info.dgjones.abora.gold.collection.basic.PrimIntegerArray;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.basic.SharedPtrArray;
import info.dgjones.abora.gold.collection.basic.UInt32Array;
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.collection.basic.WeakPtrArray;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.fbtest.BackendBootMaker;
import info.dgjones.abora.gold.fbtest.ShepherdBootMaker;
import info.dgjones.abora.gold.fbtest.WorksBootMaker;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.CxxSystemOrganization;
import info.dgjones.abora.gold.java.missing.ShepherdStub;
import info.dgjones.abora.gold.java.missing.smalltalk.AboraClass;
import info.dgjones.abora.gold.negoti8.ProtocolBroker;
import info.dgjones.abora.gold.nkernel.FeServer;
import info.dgjones.abora.gold.rcmain.SetDiskProtocol;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.snfinfo.SnarfStatistics;
import info.dgjones.abora.gold.xcvr.Binary2XcvrMaker;
import info.dgjones.abora.gold.xcvr.BogusXcvrMaker;
import info.dgjones.abora.gold.xcvr.DiskIniter;
import info.dgjones.abora.gold.xcvr.FakeDisk;
import info.dgjones.abora.gold.xcvr.TextyXcvrMaker;
import info.dgjones.abora.gold.xcvr.XcvrMaker;
import info.dgjones.abora.gold.xcvr.XnBufferedWriteStream;
import info.dgjones.abora.gold.xpp.basic.Heaper;

//TODO also need to support calling exitTimeNonInherited
public class AboraStartup {

	
	private final List aboraClasses;
	private final Map initTimeNonInheritedDependencies;
	
	private static AboraStartup INSTANCE;
	
	private static final Set IGNORE_METHODS;
	static {
		Set set = new HashSet();
		//TODO shouldn't be any methods recorded here!
		set.add("info.dgjones.abora.gold.stacker.StackExaminer.linkTimeNonInherited");

		set.add("info.dgjones.abora.gold.proman.PromiseManager.initTimeNonInherited");

//		set.add("info.dgjones.abora.gold.xcvr.Recipe.staticTimeNonInherited");
//		set.add("info.dgjones.abora.gold.cxx.classx.comm.CategoryRecipe.staticTimeNonInherited");
		IGNORE_METHODS = Collections.unmodifiableSet(set);
	}
	
	//TODO not thread safe...
	public static void startUp() throws Exception {
		if (INSTANCE == null) {
			INSTANCE = new AboraStartup(Heaper.classHierarchy(), Heaper.initTimeNonInheritedDependencies());
			INSTANCE.initialize();
			//TODO not the right place to do this...
			INSTANCE.startServerForTests();
		}
	}
	
	protected void startServerForTests() {
		AboraSupport.logger = new PrintWriter(System.out);
		
		//TODO totally running around like mad here just prodding things until something
		// interesting happens - Need to better investigate info.dgjones.abora.rcmain package - feels
		// like we are missing some bootstrap thunk definitions from a configuration
		// file
		
		// Boot makers
		//TODO not sure if we should really be running all of these?
		BootMaker worksBootMaker = new WorksBootMaker();
		Connection.registerBootPlan(worksBootMaker);
		BackendBootMaker backendBootMaker = new BackendBootMaker();
		Connection.registerBootPlan(backendBootMaker);
		ShepherdBootMaker shepherdBootMaker = new ShepherdBootMaker();
		Connection.registerBootPlan(shepherdBootMaker);
//		FeWorksBootMaker feWorksBootMaker = new FeWorksBootMaker();
//		Connection.registerBootPlan(feWorksBootMaker);

		//			AboraHeaper.CurrentPacker.fluidSet(new DiskManager());

		ProtocolBroker.registerXcvrProtocol(Binary2XcvrMaker.make());
		ProtocolBroker.registerXcvrProtocol(BogusXcvrMaker.make());
		ProtocolBroker.registerXcvrProtocol(TextyXcvrMaker.make());
		
		//TODO have no idea why I must supply my own "binary1"...		
		XcvrMaker binary1Make = new Binary2XcvrMaker() {
			public String id() {
				return "binary1";
			}
		};
		ProtocolBroker.registerXcvrProtocol(binary1Make);
		AboraHeaper.InsideTransactionFlag.fluidSet(Boolean.FALSE);
		AboraHeaper.InsideAgenda.fluidSet(Boolean.FALSE);
		
//			new Honestly().execute();
		if (((DiskManager) AboraHeaper.CurrentPacker.fluidFetch()) == null) {
			//TestPacker.make(/*blastOnError*/true, /*persistInterval*/0);
			//Turtle.make(null, /*myCategory*/null, ProtocolBroker.diskProtocol());
		}
		//AboraHeaper.CurrentGrandMap.fluidSet(HonestAbeIniter.fetchGrandMap());
		
		// Choose one of these disk initialization options
		useFakeDisk();
//		useRealDisk();
		
		//TODO due to defineFluid not fully working...
		AboraHeaper.CurrentBertCanopyCache.fluidSet(CanopyCache.make());
		AboraHeaper.CurrentSensorCanopyCache.fluidSet(CanopyCache.make());
		AboraHeaper.ActiveClubs.fluidSet(MuSet.make());
		
		//TODO not sure if I should be triggering this here?
		Connection myConnection = Connection.make(AboraSupport.findCategory(FeServer.class));

		new WorksIniter().execute();
	}

	
	public void initializeRealDisk(final String filename) {
		File file = new File(filename);
		if (file.exists() && !file.delete()) {
			throw new AboraRuntimeException("Failed to delete existing urdi: "+filename);
		}
		
		// Force text format for debugging
//		SetDiskProtocol diskProtocol = new SetDiskProtocol() {
//			{
//				myName = "texty";
//			}
//		};
//		diskProtocol.execute();

		DiskIniter diskInitier = new DiskIniter() {
			//TODO do something about the filename here...
			{
				this.myFilename = filename;
				this.myCategory = AboraSupport.findCategory(BeGrandMap.class);
			}
		};
		diskInitier.execute();
	}
	
	public void useRealDisk(final String filename) {
		dumpSnarfStatistics(filename);
		DiskManager.make(filename).getInitialFlock();
		
//		HonestAbeIniter honestAbeIniter = new HonestAbeIniter() {
//			//TODO do something properly here...
//			{
//				this.myCategory = AboraSupport.findCategory(BeGrandMap.class);
//			}
//		};
//		honestAbeIniter.execute();
		
	}

	private void dumpSnarfStatistics(final String filename) {
		PrintWriter logger = AboraSupport.logger;
		AboraSupport.logger = new PrintWriter(System.out);
		try {
			new SnarfStatistics() {
				{
					this.myFilename = filename;
				}
			}.execute();
		} finally {
			AboraSupport.logger.flush();
			AboraSupport.logger = logger;
		}
	}

	public void useFakeDisk() {
		new FakeDisk().execute();
	}

	protected AboraStartup(String[] classNames, String[][] initTimeNonInheritedDependenciesNames) throws Exception {
		super();
		aboraClasses = new ArrayList();
		Map aboraClassesLookup = populateAboraClasses(classNames);
		callAllInitializeClassAttributes();
		initializeSystemOrganization();
		
		initTimeNonInheritedDependencies = new HashMap();
		populateInitTimeNonInheritedDependencies(initTimeNonInheritedDependenciesNames, aboraClassesLookup);
	}

	private void populateInitTimeNonInheritedDependencies(String[][] initTimeNonInheritedDependenciesNames, Map aboraClassesLookup) {
		for (int i = 0; i < initTimeNonInheritedDependenciesNames.length; i++) {
			String[] strings = initTimeNonInheritedDependenciesNames[i];
			String targetClassName = strings[0];
			Set dependencies = new HashSet();
			for (int j = 1; j < strings.length; j++) {
				String dependentClassName = strings[j];
				AboraClass dependentClass = (AboraClass)aboraClassesLookup.get(dependentClassName);
				if (dependentClass != null) {
					dependencies.add(dependentClass);
				} else {
					System.out.println("Failed to find dependent class: "+dependentClassName+" for: "+targetClassName);
				}
			}
			if (!dependencies.isEmpty()) {
				initTimeNonInheritedDependencies.put(aboraClassesLookup.get(targetClassName), dependencies);
			}
		}
	}

	private void callAllInitializeClassAttributes() throws IllegalAccessException, InvocationTargetException {
		for (Iterator iter = aboraClasses.iterator(); iter.hasNext();) {
			AboraClass aboraClass = (AboraClass) iter.next();
			Class aClass = aboraClass.getJavaClass();
			try {
				Method method = aClass.getDeclaredMethod("initializeClassAttributes", null);
				method.invoke(null, null);
			} catch (NoSuchMethodException e) {
				System.out.println("No initializeClassAttributes for class: "+aClass);
			}
		}
	}

	private Map populateAboraClasses(String[] classNames) throws ClassNotFoundException {
		Map aboraClassesLookup = new HashMap();
		for (int i = 0; i < classNames.length; i++) {
			String className = classNames[i];
			Class c = Class.forName(className);
			AboraClass aboraClass = AboraClass.findAboraClass(c);
			aboraClasses.add(aboraClass);
			aboraClassesLookup.put(className, aboraClass);
		}
		return aboraClassesLookup;
	}

	private void initializeSystemOrganization() {
		XnBufferedWriteStream.initializeSystemOrganization();
		
		// Manual initialization required for "missing" classes 
		CxxSystemOrganization.getOrMakeFileNamed("sheph").addClassIn(AboraSupport.findAboraClass(ShepherdStub.class).getClassDescription(), Heaper.PUBLIC);
		//TODO array assumed...
		CxxSystemOrganization.getOrMakeFileNamed("array")
			.addClassIn(AboraSupport.findAboraClass(IEEE32Array.class).getClassDescription(), Heaper.PUBLIC)
			.addClassIn(AboraSupport.findAboraClass(IEEE64Array.class).getClassDescription(), Heaper.PUBLIC)
			.addClassIn(AboraSupport.findAboraClass(Int32Array.class).getClassDescription(), Heaper.PUBLIC)
			.addClassIn(AboraSupport.findAboraClass(Int8Array.class).getClassDescription(), Heaper.PUBLIC)
			.addClassIn(AboraSupport.findAboraClass(IntegerVarArray.class).getClassDescription(), Heaper.PUBLIC)
			.addClassIn(AboraSupport.findAboraClass(PrimArray.class).getClassDescription(), Heaper.PUBLIC)
			.addClassIn(AboraSupport.findAboraClass(PrimDataArray.class).getClassDescription(), Heaper.PUBLIC)
			.addClassIn(AboraSupport.findAboraClass(PrimFloatArray.class).getClassDescription(), Heaper.PUBLIC)
			.addClassIn(AboraSupport.findAboraClass(PrimIntArray.class).getClassDescription(), Heaper.PUBLIC)
			.addClassIn(AboraSupport.findAboraClass(PrimIntegerArray.class).getClassDescription(), Heaper.PUBLIC)
			.addClassIn(AboraSupport.findAboraClass(PtrArray.class).getClassDescription(), Heaper.PUBLIC)
			.addClassIn(AboraSupport.findAboraClass(SharedPtrArray.class).getClassDescription(), Heaper.PUBLIC)
			.addClassIn(AboraSupport.findAboraClass(UInt32Array.class).getClassDescription(), Heaper.PUBLIC)
			.addClassIn(AboraSupport.findAboraClass(UInt8Array.class).getClassDescription(), Heaper.PUBLIC)
			.addClassIn(AboraSupport.findAboraClass(WeakPtrArray.class).getClassDescription(), Heaper.PUBLIC)
			;
		
	}
	
	protected void initialize() throws IllegalArgumentException, IllegalAccessException, InvocationTargetException, SecurityException, NoSuchMethodException {
		initialize("linkTimeNonInherited");
		initialize("staticTimeNonInherited");
		initialize("initTimeNonInherited", initTimeNonInheritedDependencies);
	}

	protected void initialize(String methodName) throws SecurityException, IllegalArgumentException, IllegalAccessException, InvocationTargetException {
		initialize(methodName, Collections.EMPTY_MAP);
	}
	protected void initialize(String methodName, Map allDependencies) throws SecurityException, IllegalArgumentException, IllegalAccessException, InvocationTargetException {
		Set alreadyInitialized = new HashSet();
		for (Iterator iter = aboraClasses.iterator(); iter.hasNext();) {
			AboraClass element = (AboraClass) iter.next();
			initialize(element, methodName, allDependencies, alreadyInitialized);
		}
	}

	protected void initialize(AboraClass element, String methodName, Map allDependencies, Set alreadyInitialized) throws IllegalAccessException, InvocationTargetException {		
		Class c = element.getJavaClass();
		if (alreadyInitialized.contains(c)) {
			return;
		}
		
		Set dependencies = (Set)allDependencies.get(element);
		if (dependencies != null) {
			for (Iterator iterator = dependencies.iterator(); iterator.hasNext();) {
				AboraClass dependent = (AboraClass) iterator.next();
				initialize(dependent, methodName, allDependencies, alreadyInitialized);
			}
		}
		initialize(c, methodName);
		alreadyInitialized.add(c);
	}
	
	private void initialize(Class c, String methodName) throws IllegalAccessException, InvocationTargetException {
		Method method = getMethodIfAny(c, methodName);
		if (method != null) {
			if (!IGNORE_METHODS.contains(c.getName()+"."+methodName)) {
			
				try {
					method.invoke(null, null);
				} catch (InvocationTargetException e) {
					System.out.println("Failed to initialize: "+c+"."+methodName+" due to: "+e.getCause());
					throw e;
				}
			} else {
				System.out.println("Ignoring initialize: "+c+"."+methodName);
			}
		}
	}
	
	protected Method getMethodIfAny(Class c, String methodName) {
		for (int i = 0; i < c.getDeclaredMethods().length; i++) {
			Method method = c.getDeclaredMethods()[i];
			if (method.getName().equals(methodName)) {
				return method;
			}
		}
		return null;
	}

	public static AboraStartup getInstance() throws Exception {
		startUp();
		return INSTANCE;
	}

}
