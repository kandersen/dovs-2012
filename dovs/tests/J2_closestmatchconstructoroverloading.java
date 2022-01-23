// TYPE_CHECKING
// JOOS1: NO_MATCHING_CONSTRUCTOR_FOUND,JOOS1_CLOSEST_MATCH_OVERLOADING
public class J2_closestmatchconstructoroverloading {
    public int z;
    public J2_closestmatchconstructoroverloading() {
	this.z = 1024;
    }
    public J2_closestmatchconstructoroverloading(Object x, Object y) {
	this.z = -42;
    }
    public J2_closestmatchconstructoroverloading(Object x, J2_closestmatchconstructoroverloading y) {
	this.z = 123;
    }
    public static int test() {
	return new J2_closestmatchconstructoroverloading(new J2_closestmatchconstructoroverloading(), 
							 new J2_closestmatchconstructoroverloading()).z;
	
    }

}
