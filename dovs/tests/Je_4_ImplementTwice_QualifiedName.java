// JOOS1:HIERARCHY,REPEATED_INTERFACE
// JOOS2:HIERARCHY,REPEATED_INTERFACE
// JAVAC:UNKNOWN
// 
/**
 * Hierarchy:
 * - An interface must not be mentioned more than once in the same
 * implements clause of a class (8.1.4, simple constraint 3).
 */
import java.io.*;

public class Je_4_ImplementTwice_QualifiedName implements Runnable, Serializable, Cloneable, java.io.Serializable{

    public Je_4_ImplementTwice_QualifiedName(){}

    public static int test(){
	return 123;
    }

    public void run(){}
    
}
