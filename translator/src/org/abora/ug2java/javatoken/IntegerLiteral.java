package org.abora.ug2java.javatoken;




public class IntegerLiteral extends JavaLiteral {

	private final long longValue;
	private final int radix;
	
	public IntegerLiteral(long longValue) {
		this(longValue, 10);
	}

	public IntegerLiteral(long longValue, int radix) {
		super(generateString(longValue, radix));
		this.longValue = longValue;
		this.radix = radix;
	}

	public int getIntValue() {
		return (int)longValue;
	}

	public long getLongValue() {
		return longValue;
	}
	
	public int getRadix() {
		return radix;
	}
	
	
	private static String generateString(long longValue, int radix) {
		String generate = "";
		boolean outsideIntegerRange = longValue > Integer.MAX_VALUE || longValue < Integer.MIN_VALUE;
		
		if (radix == 16) {
			generate += "0x";
			generate += outsideIntegerRange ? Long.toHexString(longValue) : Integer.toHexString((int)longValue); 
		} else if (radix == 8) {
			generate += "0";
			generate += outsideIntegerRange ? Long.toOctalString(longValue) : Integer.toOctalString((int)longValue);
		} else {
			radix = 10;
			generate += Long.toString(longValue);
		}
		
		if (outsideIntegerRange) {
			generate +="L";
		}
		return generate;
	}

}
