// TYPE_CHECKING

import java.awt.font.GlyphJustificationInfo;

public class J1_6_FinalFieldAccess_Instance {
	public J1_6_FinalFieldAccess_Instance(GlyphJustificationInfo info) {
		int growPriority = 0;
		growPriority = info.growPriority; // GlyphJustificationInfo.growPriority is final
	}
	
	public static int test() {
		return 123;
	}
}
		