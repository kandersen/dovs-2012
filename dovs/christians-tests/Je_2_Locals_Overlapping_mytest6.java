// ENVIRONMENTS
// JOOS1:ENVIRONMENTS,DUPLICATE_VARIABLE
// JOOS2:ENVIRONMENTS,DUPLICATE_VARIABLE
// JAVAC:UNKNOWN
// 
/**
 * Environments:
 * - Check that no two local variables with overlapping scope have the
 * same name.
 */
public class Je_2_Locals_Overlapping_mytest6 {

    public Je_2_Locals_Overlapping_mytest6() {}

    public static int test() {
      int r = 0;
      if(r == 0)
        while(r == 1){
          boolean r = false;
        }
      return 123;
    }
}