package org.abora.ug2java.util;

public class ClassHelper {

	private ClassHelper() {
		// Do not instantiate - static helper
	}

	/**
	 * Return the name of the class without package.
	 */
	public static String getShortName(Class aClass) {
		String fullName = aClass.getName();
		return getShortName(fullName);
	}

	/**
	 * Return the name of the class without package.
	 */
	public static String getShortName(String fullClassName) {
		return fullClassName.substring(fullClassName.lastIndexOf(".") + 1);
	}
}