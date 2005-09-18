package org.abora.gold.java.missing;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import org.abora.gold.cxx.otherclass.CopyRecipe;
import org.abora.gold.java.exception.AboraRuntimeException;
import org.abora.gold.java.missing.smalltalk.AboraClass;
import org.abora.gold.java.missing.smalltalk.Association;
import org.abora.gold.xcvr.Rcvr;
import org.abora.gold.xcvr.Recipe;
import org.abora.gold.xcvr.SpecialistRcvr;
import org.abora.gold.xpp.basic.Heaper;


public class PseudoCopyRecipe extends CopyRecipe {

	public PseudoCopyRecipe(AboraClass cat, Association cuisine) {
		super(cat.getCategory(), cuisine);
		// TODO Auto-generated constructor stub
	}

	protected static Recipe associationToRecipe(Association assoc) {
		return (Recipe)assoc.value();
	}


	public PseudoCopyRecipe() {
		super();
		// TODO Auto-generated constructor stub
	}

	public PseudoCopyRecipe(Rcvr receiver) {
		super(receiver);
		// TODO Auto-generated constructor stub
	}

	//TODO override to simply get something to work here
	public Heaper parse(SpecialistRcvr rcvr) {
		try {
		Class receiverClass = categoryOfDish().brotherClass().getJavaClass();
		Constructor constructor = receiverClass.getConstructor(new Class[] {Rcvr.class});
//		rcvr.registerIbid(heaper); // TODO pushed into Heaper(Rcvr) - probably totally wrong!
		Heaper heaper = (Heaper) constructor.newInstance(new Object[] {rcvr});
		//TODO receive heaper.receive(rcvr);
		return heaper;
		} catch (InvocationTargetException e) {
			throw new AboraRuntimeException("Failed to call constructor for new object: "+rcvr);
		} catch (NoSuchMethodException e) {
			throw new AboraRuntimeException("Failed to find constructor for new object: "+rcvr);
		} catch (IllegalAccessException e) {
			throw new AboraRuntimeException("Failed to access new object: "+rcvr);
		} catch (InstantiationException e) {
			throw new AboraRuntimeException("Failed to instantiate: "+rcvr);
		}
	
//		Heaper result = ;
////		AboraSupport.translateOnly();
//		{
//			/*  
//		result = Heaper::operator new (0, xcsj, this->categoryOfDish()); */
//		}
////		AboraSupport.smalltalkOnly();
//		{
//			result = new DeletedHeaper();
//		}
//		rcvr.registerIbid((result));
//		parseInto(rcvr, (PtrArray) result);
//		return result;
//
	}

}
