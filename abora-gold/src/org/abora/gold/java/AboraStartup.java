package org.abora.gold.java;

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

import org.abora.gold.be.canopy.CanopyCache;
import org.abora.gold.cobbler.BootMaker;
import org.abora.gold.cobbler.Connection;
import org.abora.gold.collection.sets.MuSet;
import org.abora.gold.fbtest.BackendBootMaker;
import org.abora.gold.fbtest.WorksBootMaker;
import org.abora.gold.java.missing.smalltalk.AboraClass;
import org.abora.gold.negoti8.ProtocolBroker;
import org.abora.gold.snarf.DiskManager;
import org.abora.gold.snarf.FakePacker;
import org.abora.gold.snarf.MockTurtle;
import org.abora.gold.xcvr.Binary2XcvrMaker;
import org.abora.gold.xcvr.BogusXcvrMaker;
import org.abora.gold.xcvr.TextyXcvrMaker;
import org.abora.gold.xcvr.XcvrMaker;
import org.abora.gold.xpp.basic.Heaper;


public class AboraStartup {

	private final List aboraClasses;
	private final Map initTimeNonInheritedDependencies;
	
	private static AboraStartup INSTANCE;
	
	private static final Set IGNORE_METHODS;
	static {
		Set set = new HashSet();
		//TODO shouldn't be any methods recorded here!
		set.add("org.abora.gold.stacker.StackExaminer.linkTimeNonInherited");

		set.add("org.abora.gold.proman.PromiseManager.initTimeNonInherited");
		//set.add("org.abora.gold.lock.NoEncrypter.initTimeNonInherited");

		set.add("org.abora.gold.xcvr.Recipe.staticTimeNonInherited");
		set.add("org.abora.gold.cxx.classx.comm.CategoryRecipe.staticTimeNonInherited");
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
		
		BootMaker worksBootMaker = new WorksBootMaker();
		Connection.registerBootPlan(worksBootMaker);
		BackendBootMaker backendBootMaker = new BackendBootMaker();
		Connection.registerBootPlan(backendBootMaker);
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
			FakePacker.make();
			MockTurtle.make(null);
		}
		//AboraHeaper.CurrentGrandMap.fluidSet(HonestAbeIniter.fetchGrandMap());
		
		//TODO due to defineFluid not fully working...
		AboraHeaper.CurrentBertCanopyCache.fluidSet(CanopyCache.make());
		AboraHeaper.CurrentSensorCanopyCache.fluidSet(CanopyCache.make());
		AboraHeaper.ActiveClubs.fluidSet(MuSet.make());
	}

	protected AboraStartup(String[] classNames, String[][] initTimeNonInheritedDependenciesNames) throws Exception {
		super();
		aboraClasses = new ArrayList();
		Map aboraClassesLookup = new HashMap();
		for (int i = 0; i < classNames.length; i++) {
			String className = classNames[i];
			Class c = Class.forName(className);
			AboraClass aboraClass = new AboraClass(c);
			aboraClasses.add(aboraClass);
			aboraClassesLookup.put(className, aboraClass);
		}
		initTimeNonInheritedDependencies = new HashMap();
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
	
	protected void initialize() throws IllegalArgumentException, IllegalAccessException, InvocationTargetException, SecurityException, NoSuchMethodException {
		initialize("linkTimeNonInherited");
		initialize("initTimeNonInherited", initTimeNonInheritedDependencies);
		initialize("staticTimeNonInherited");
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
	
//	public static void initializeSystem() {
//		if (initialized) {
//			return;
//		}
//		
//		BeClub.staticTimeNonInherited();
//		BeGrandMap.staticTimeNonInherited();
//		BertCrum.staticTimeNonInherited();
//		SensorCrum.staticTimeNonInherited();
//		Ent.staticTimeNonInherited();
//		//TODOCategoryRecipe.staticTimeNonInherited();
//		WorksBootMaker.staticTimeNonInherited();
//		FePromiseSession.staticTimeNonInherited();
//		FeSession.staticTimeNonInherited();
//		FeServer.staticTimeNonInherited();
//		WorksTester.staticTimeNonInherited();
//		MainDummy.staticTimeNonInherited();
//		ServerLoop.staticTimeNonInherited();
//		Abraham.staticTimeNonInherited();
//		DiskManager.staticTimeNonInherited();
//		FlockInfo.staticTimeNonInherited();
//		//TODORecipe.staticTimeNonInherited();
//		
//		initialized = true;
//	}

}
