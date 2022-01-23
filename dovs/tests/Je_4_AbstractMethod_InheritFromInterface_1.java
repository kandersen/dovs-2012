// JOOS1:HIERARCHY,CLASS_MUST_BE_ABSTRACT
// JOOS2:HIERARCHY,CLASS_MUST_BE_ABSTRACT
// JAVAC:UNKNOWN
// 
/**
 * Hierarchy check:
 * - A class that has (declares or inherits) any abstract methods must
 * be abstract (8.1.1.1). (method run() not implemented)
 */
public class Je_4_AbstractMethod_InheritFromInterface_1 implements Runnable{

    public Je_4_AbstractMethod_InheritFromInterface_1(){
    }

    public static int test(){
	return 123;
    }

}
