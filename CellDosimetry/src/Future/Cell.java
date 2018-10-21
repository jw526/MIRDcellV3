package Future;

/**
 * Created by Alex on 6/8/2017.
 */
public class Cell {
	// replaces the original cell array
	public final int x, y, z;           // cell[cell number][1/2/3]
	public boolean labled = false;     // cell[cell number][4]
	public double activity = 0;        // cell[cell number][5]
	public double selfDose = 0;        // cell[cell number][6]
	public double crossdose;           // cell[cell number][7]

	// the above cross dose for regular calculations would be replaced with the below for complex calculations
	public double[][] crossDoses;      // [ICODE][REGION] where ICODE and REGION are enums defined in the ENUMS class

	public boolean alive = true;
	public double survival = 1;

	public Cell(int x, int y, int z){
		this.x = x;
		this.y = y;
		this.z = z;
	}

}

