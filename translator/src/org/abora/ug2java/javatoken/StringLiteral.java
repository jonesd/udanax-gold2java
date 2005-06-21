package org.abora.ug2java.javatoken;



public class StringLiteral extends JavaLiteral {

	private final String stringValue;
	
	public StringLiteral(String stringValue) {
		super(generateValue(stringValue));
		this.stringValue = stringValue;
	}
	
	private static String generateValue(String stringValue) {
		return "\""+stringValue+"\"";
	}
	
	public String getStringValue() {
		return stringValue;
	}

}
