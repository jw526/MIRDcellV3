package WebDose;

import javax.swing.*;
import javax.swing.text.NumberFormatter;
import java.awt.Graphics;
import java.awt.Color;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Random;
import java.util.Arrays;

public class SurvivalCalculationComplex {

	public double activitytotal;
	public double tempsurvall, tempsurvlabel, tempsurvunlabel;
	public double[][] PlotOutput;
	double SurvFrac;
	private final double step = 100D; // change this number to make the surviving fraction curve more accurate / have a more fine step ratio
	public String output;

	public SurvivalCalculationComplex() {
		activitytotal = 0;
		tempsurvall = 0;
		tempsurvlabel = 0;
		tempsurvunlabel = 0;
		PlotOutput = new double[8][(int) step + 1];
		SurvFrac = 0;

		//PlotOutput[7][0] = 1;
		//PlotOutput[6][0] = 1;
		//PlotOutput[5][0] = 1;
	}

	public double[][] calculateSurvival(
			double[][] cell, double[][] celllabel, double[][][] SValues, double[][] selfSValues, double[][] complexRadiobiologicalParams, double[] activityFractions,
			double MAC, int d, int rCell,
			int cellnumber, int jHeight, int jWidth, int radiationtarget,
			JTextArea jTextArea5,
			Graphics progress
	) {
		double dist;
		double labelRad = 0D;   // A counter for the radiation dose to all the labeled cells
		double MeanABD;         // Average dose to each cell
		double MeanABDL;        // Average dose to all cells
		double MeanABDUL = 0D;  // Average dose to unlabled cells
		double percent;         // The perccent completion for all the progress bar
		double rand;
		double ratio;
		double survFrac = 0D;   // the field that keeps track of the surviving fraction
		//double Survival;      // the field where the Pval is stored (probability of survival between 0 and 1)
		double totalRad = 0D;   // A counter for the radiation dose of all the cells
		double x, y, z;         // The cells' position in the cluster

		output = "MAC(Bq)\tMDC(all)\tMDC(γ)\tMDC(X)\tMDC(AQ)\tMDC(β+)\tMDC(β-)\tMDC(IE)\tMDC(Auger)\tMDC(α)\tMDC(αR)\tMDC(FF)\tMDC(N)\tMALC(Bq)\tMDLC(all)\tMDLC(γ)\tMDLC(X)\tMDLC(AQ)\tMDLC(β+)\tMDLC(β-)\tMDLC(IE)\tMDLC(Auger)\tMDLC(α)\tMDLC(αR)\tMDLC(FF)\tMDLC(N)\t MDUC(all)\tMDUC(γ)\tMDUC(X)\tMDUC(AQ)\tMDUC(β+)\tMDUC(β-)\tMDUC(IE)\tMDUC(Auger)\tMDUC(α)\tMDUC(αR)\tMDUC(FF)\tMDUC(N)\tSF(labeled)\tSF(unlabeled)\tSF(all cells)" + "\n";

		// TODO see below
		// clear out a huge block og contiguous memory.  this is a terrible way to do this but i dont have the time to code a doubly linked list of dose objects
		double[][] crossDoses = new double[cellnumber][11];             // [index][ICODE]
		double[] Survival = new double[cellnumber];                     // the field where the Pval is stored (probability of survival between 0 and 1)
		Arrays.fill(Survival, 1.0);

		//int labelLength = celllabel.length - 1; // = just put this in manually
		int labelCell = celllabel.length;           // the number of labled cells
		int unlableCell = cellnumber - labelCell;   // the number of unlabled cells
		//int length = cell.length - 1; // = cellnumber
		int allLive = 0;                            // the number of cells that live
		int labeledLive = 0;                              // the number of labled cells that live
		int unlabeledLive;                           // the number of unlabled cells that live
		int PlotPoints = 0;
		
		boolean[] coldLiving = new boolean[cellnumber];
		for(int i = 0; i < cellnumber; i++){
			if(cell[i][0] != 1){
				coldLiving[i] = true;
				cell[i][0] = 1;
			}
			else{
				coldLiving[i] = false;
			}
		}

		Random random = new Random();

		// prepare the progress bar for part 2 of 2
		progress.clearRect(0, 0, jWidth, jHeight);
		progress.setColor(Color.BLACK);
		progress.drawString("Part 2 of 2 : 0%", jWidth / 2 - 50, jHeight / 2 + 6);
		progress.setColor(Color.GREEN);
		progress.fillRect(1, 1, 1, jHeight);

		/*
		 * SValues[][][] = [ICODE][distance][dose region]
		 * selfSVals[][] = [ICODE][dose region]
		 * for dose regions:
		 *      0 = cell to cell;
		 *      1 = cell surface to cell
		 *      2 = Nucleus to Nucleus
		 *      3 = Cytoplasm to Nucleus
		 *      4 = Cell Surface to Nucleus
		 *      5 = Nucleus to Cytoplasm
		 *      7 = Cell Surface to Cytoplasm
		 *      6 = Cytoplasm to Cytoplasm
		 */


		if (radiationtarget == 0) {
			// radiation target: cell
			for (int i = 0; i < cellnumber; i++) {
				/* for each cell */
				if (cell[i][4] != 0) {
					/* if that cell is labeled with radiation calculate self dose */
					for (int ICODE = 0; ICODE < 11; ICODE++) {
						/* self, C->C */
						if (complexRadiobiologicalParams[ICODE][0] != 0 || complexRadiobiologicalParams[ICODE][1] != 0) {
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][0] * selfSValues[ICODE + 1][0] * activityFractions[0] * cell[i][5] - complexRadiobiologicalParams[ICODE][1] * selfSValues[ICODE + 1][0] * selfSValues[ICODE + 1][0] * activityFractions[0] * activityFractions[0] * cell[i][5] * cell[i][5]);
							totalRad += selfSValues[ICODE + 1][0] * activityFractions[2] * cell[i][5];
							labelRad += selfSValues[ICODE + 1][0] * activityFractions[2] * cell[i][5];
						}
						/* self, CS->C */
						if (complexRadiobiologicalParams[ICODE][2] != 0 || complexRadiobiologicalParams[ICODE][3] != 0) {
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][2] * selfSValues[ICODE + 1][1] * activityFractions[3] * cell[i][5] - complexRadiobiologicalParams[ICODE][3] * selfSValues[ICODE + 1][1] * selfSValues[ICODE + 1][1] * activityFractions[3] * activityFractions[3] * cell[i][5] * cell[i][5]);
							totalRad += selfSValues[ICODE + 1][1] * activityFractions[3] * cell[i][5];
							labelRad += selfSValues[ICODE + 1][1] * activityFractions[3] * cell[i][5];
						}
					}
				}

				// then dose that cell feom each labeled cell
				for (int ICODE = 0; ICODE < 11; ICODE++) {
					for (int j = 0; j < labelCell; j++) {
						x = celllabel[j][1] - cell[i][1];
						y = celllabel[j][2] - cell[i][2];
						z = celllabel[j][3] - cell[i][3];
						dist = Math.sqrt(x * x + y * y + z * z);

						if (dist == 0){
							continue;
						}

						if (dist > SValues.length - 1) {
							dist = SValues.length - 1;
						}

						crossDoses[i][ICODE] += SValues[ICODE + 1][(int)dist - 2 * rCell][0] * celllabel[j][5] * activityFractions[2]; /* cross, C->C */
						crossDoses[i][ICODE] += SValues[ICODE + 1][(int)dist - 2 * rCell][1] * celllabel[j][5] * activityFractions[3]; /* cross, CS->C */
						cell[i][7] += crossDoses[i][ICODE];
					}

					if (complexRadiobiologicalParams[ICODE][10] != 0 || complexRadiobiologicalParams[ICODE][11] != 0) {
						Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][10] * crossDoses[i][ICODE] - complexRadiobiologicalParams[ICODE][11] * crossDoses[i][ICODE] * crossDoses[i][ICODE]);
						totalRad += crossDoses[i][ICODE];
						if (cell[i][4] != 0) {
							labelRad += crossDoses[i][ICODE];
						}
					}
				}

				rand = random.nextDouble();
				if (rand >= Survival[i] || coldLiving[i]) {
					cell[i][0] = 0;
				} else {
					allLive++;
					if (cell[i][4] != 0) {
						labeledLive++;
					}
				}

				//update the progress bar
				percent = ((double) i / (double) cellnumber) * (double) jWidth;
				progress.clearRect(0, 0, jWidth, jHeight);
				progress.setColor(Color.GREEN);
				progress.fillRect(1, 1, (int) percent, 28);
				progress.setColor(Color.BLACK);
				progress.drawString("Part 2 of 2 : " + String.format("%1$.2f", percent / jWidth * 100) + "%", jWidth / 2 - 50, jHeight / 2 + 6);
			}

			MeanABD = totalRad / cellnumber;
			MeanABDL = labelRad / labelCell;
			if ((cellnumber - labelCell) != 0) {
				MeanABDUL = (totalRad - labelRad) / (cellnumber - labelCell);
			}
			System.err.println("MeanABDUL = " + MeanABDUL);
			System.err.println("totalRad = " + totalRad);
			System.err.println("labelRad = " + labelRad);

			unlabeledLive = allLive - labeledLive;

			for (int i = cell.length - 1; i >= 0; i--) {
				activitytotal = activitytotal + cell[i][5];
			}

			progress.clearRect(0, 0, jWidth, jHeight);
			progress.setColor(Color.GREEN);
			progress.fillRect(1, 1, jWidth, jHeight);
			progress.setColor(Color.BLACK);
			progress.drawString("Plotting Data - please wait", jWidth / 2 - 50, jHeight / 2 + 6);

			double[][] totalDoses = new double[11][3]; // totalDoses[ICODE][mixed | labeled | unlabeled]
			for (int i = 0; i < cellnumber; i++) {
				for (int ICODE = 1; ICODE < 12; ICODE++) {
					totalDoses[ICODE - 1][0] += crossDoses[i][ICODE - 1] + selfSValues[ICODE][0] * activityFractions[2] * cell[i][5] + selfSValues[ICODE][1] * activityFractions[3] * cell[i][5];
					if(cell[i][4] != 0) {
						totalDoses[ICODE - 1][1] += crossDoses[i][ICODE - 1] + selfSValues[ICODE][0] * activityFractions[2] * cell[i][5] + selfSValues[ICODE][1] * activityFractions[3] * cell[i][5];
					}
					else {
						totalDoses[ICODE - 1][2] += crossDoses[i][ICODE - 1];
					}
				}
			}

			for (double k = 1D; k <= step; k++) {
				Arrays.fill(Survival, 1);
				allLive = 0;
				labeledLive = 0;
				unlabeledLive = 0;
				ratio = k / step;

				for (int i = 0; i < cellnumber; i++) {
					for (int ICODE = 0; ICODE < 11; ICODE++) {
						/* self, C->C */
						if (complexRadiobiologicalParams[ICODE][0] != 0 || complexRadiobiologicalParams[ICODE][1] != 0)
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][0] * ratio * selfSValues[ICODE + 1][0] * activityFractions[2] * cell[i][5] - complexRadiobiologicalParams[ICODE][1] * ratio * ratio * selfSValues[ICODE + 1][0] * selfSValues[ICODE + 1][0] * activityFractions[2] * activityFractions[2] * cell[i][5] * cell[i][5]);
						/* self, CS->C */
						if (complexRadiobiologicalParams[ICODE][2] != 0 || complexRadiobiologicalParams[ICODE][3] != 0)
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][2] * ratio * selfSValues[ICODE + 1][1] * activityFractions[2] * cell[i][5] - complexRadiobiologicalParams[ICODE][3] * ratio * ratio * selfSValues[ICODE + 1][1] * selfSValues[ICODE + 1][1] * activityFractions[3] * activityFractions[3] * cell[i][5] * cell[i][5]);
						/* cross, C->C & CS->C */
						if (complexRadiobiologicalParams[ICODE][4] != 0 || complexRadiobiologicalParams[ICODE][5] != 0)
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][4] * ratio * crossDoses[i][ICODE] - complexRadiobiologicalParams[ICODE][5] * ratio * ratio * crossDoses[i][ICODE] * crossDoses[i][ICODE]);
					}

					rand = random.nextDouble();
					if (rand < Survival[i] && !coldLiving[i]) {
						allLive++;
						if (cell[i][4] != 0) {
							unlabeledLive++;
						} else {
							labeledLive++;
						}
					}
				}

				tempsurvall = (double) allLive / (double) cellnumber;
				if (labelCell != 0) {
					tempsurvlabel = (double) labeledLive / (double) labelCell;
				}
				if (unlableCell != 0) {
					tempsurvunlabel = (double) unlabeledLive / (double) unlableCell;
				}

				NumberFormat nf = new DecimalFormat("0.00E00");
				output += ""
						+ nf.format( (MAC * ratio) ) + "\t"
						+ nf.format( (MeanABD * ratio) ) + "\t"
						+ nf.format(totalDoses[0][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[1][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[2][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[3][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[4][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[5][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[6][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[7][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[8][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[9][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[10][0] / cellnumber * ratio ) + "\t"
						+ nf.format(MAC * ratio * cellnumber / labelCell) + "\t"
						+ nf.format(MeanABDL * ratio) + "\t"
						+ nf.format(totalDoses[0][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[1][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[2][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[3][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[4][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[5][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[6][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[7][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[8][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[9][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[10][1] / cellnumber * ratio ) + "\t"
						+ nf.format(MeanABDUL * ratio) + "\t"
						+ nf.format(totalDoses[0][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[1][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[2][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[3][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[4][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[5][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[6][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[7][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[8][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[9][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[10][2] / cellnumber * ratio ) + "\t"
						+ nf.format(tempsurvlabel) + "\t"
						+ nf.format(tempsurvunlabel) + "\t"
						+ nf.format(tempsurvall) + "\n";

				PlotOutput[0][PlotPoints] = (MAC * ratio);                          //MAC
				PlotOutput[1][PlotPoints] = (MeanABD * ratio);                      //MDC
				PlotOutput[2][PlotPoints] = (MAC * ratio * cellnumber / labelCell); //MALC
				PlotOutput[3][PlotPoints] = (MeanABDL * ratio);                     //MDLC
				PlotOutput[4][PlotPoints] = (MeanABDUL * ratio);                    //MDULC
				PlotOutput[5][PlotPoints] = (tempsurvlabel);                        //SF of Labeled
				PlotOutput[6][PlotPoints] = (tempsurvunlabel);                      //SF of unlabeled
				PlotOutput[7][PlotPoints] = (tempsurvall);                          //SF of all

				PlotPoints++;
			}
			totalDoses = null; // free up some memory space
		} else if (radiationtarget == 1) {
			// radiation target: Nucleus
			for (int i = 0; i < cellnumber; i++) {
				if (cell[i][4] != 0) {
					for (int ICODE = 0; ICODE < 11; ICODE++) {
						/* self, N->N */
						if (complexRadiobiologicalParams[ICODE][0] != 0 || complexRadiobiologicalParams[ICODE][1] != 0) {
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][0] * selfSValues[ICODE + 1][2] * activityFractions[1] * cell[i][5] - complexRadiobiologicalParams[ICODE][1] * selfSValues[ICODE + 1][2] * activityFractions[1] * cell[i][5] * selfSValues[ICODE + 1][2] * activityFractions[1] * cell[i][5]);
							totalRad += selfSValues[ICODE + 1][2] * activityFractions[0] * cell[i][5];
							labelRad += selfSValues[ICODE + 1][2] * activityFractions[0] * cell[i][5];
						}
						/* self, Cy->N */
						if (complexRadiobiologicalParams[ICODE][2] != 0 || complexRadiobiologicalParams[ICODE][3] != 0) {
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][2] * selfSValues[ICODE + 1][3] * activityFractions[2] * cell[i][5] - complexRadiobiologicalParams[ICODE][3] * selfSValues[ICODE + 1][3] * activityFractions[2] * cell[i][5] * selfSValues[ICODE + 1][3] * activityFractions[2] * cell[i][5]);
							totalRad += selfSValues[ICODE + 1][3] * activityFractions[2] * cell[i][5];
							labelRad += selfSValues[ICODE + 1][3] * activityFractions[2] * cell[i][5];
						}
						/* se;f, CS->N */
						if (complexRadiobiologicalParams[ICODE][4] != 0 || complexRadiobiologicalParams[ICODE][5] != 0) {
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][2] * selfSValues[ICODE + 1][4] * activityFractions[3] * cell[i][5] - complexRadiobiologicalParams[ICODE][5] * selfSValues[ICODE + 1][4] * activityFractions[3] * cell[i][5] * selfSValues[ICODE + 1][4] * activityFractions[3] * cell[i][5]);
							totalRad += selfSValues[ICODE + 1][4] * activityFractions[3] * cell[i][5];
							labelRad += selfSValues[ICODE + 1][4] * activityFractions[3] * cell[i][5];
						}
					}
				}

				for (int ICODE = 0; ICODE < 11; ICODE++) {
					for (int j = 0; j < labelCell; j++) {
						x = celllabel[j][1] - cell[i][1];
						y = celllabel[j][2] - cell[i][2];
						z = celllabel[j][3] - cell[i][3];
						dist = Math.sqrt(x * x + y * y + z * z);

						if(dist == 0){
							continue;
						}

						if (dist > SValues[0].length - 1) {
							dist = SValues[0].length - 1;
						}

						crossDoses[i][ICODE] += SValues[ICODE + 1][(int)dist - 2 * rCell][2] * celllabel[j][5] * activityFractions[1]; /* cross, N->N */
						crossDoses[i][ICODE] += SValues[ICODE + 1][(int)dist - 2 * rCell][3] * celllabel[j][5] * activityFractions[2]; /* cross, Cy->N */
						crossDoses[i][ICODE] += SValues[ICODE + 1][(int)dist - 2 * rCell][4] * celllabel[j][5] * activityFractions[3]; /* cross, CS->N */
						cell[i][7] += crossDoses[i][ICODE];
					}

					if (complexRadiobiologicalParams[ICODE][10] != 0 || complexRadiobiologicalParams[ICODE][11] != 0) {
						Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][10] * crossDoses[i][ICODE] - complexRadiobiologicalParams[ICODE][11] * crossDoses[i][ICODE] * crossDoses[i][ICODE]);
						totalRad += crossDoses[i][ICODE];
						if (cell[i][4] != 0) {
							labelRad += crossDoses[i][ICODE];
						}
					}
				}

				rand = random.nextDouble();
				if (rand >= Survival[i] || coldLiving[i]) {
					cell[i][0] = 0;
				} else {
					allLive++;
					if (cell[i][4] != 0) {
						labeledLive++;
					}
				}

				//update the progress bar
				percent = ((double) i / (double) cellnumber) * (double) jWidth;
				progress.clearRect(0, 0, jWidth, jHeight);
				progress.setColor(Color.GREEN);
				progress.fillRect(1, 1, (int) percent, 28);
				progress.setColor(Color.BLACK);
				progress.drawString("Part 2 of 2 : " + String.format("%1$.2f", percent / jWidth * 100) + "%", jWidth / 2 - 50, jHeight / 2 + 6);
			}

			MeanABD = totalRad / cellnumber;
			MeanABDL = labelRad / labelCell;
			if ((cellnumber - labelCell) != 0) {
				MeanABDUL = (totalRad - labelRad) / (cellnumber - labelCell);
			}
			System.err.println("MeanABDUL = " + MeanABDUL);
			System.err.println("totalRad = " + totalRad);
			System.err.println("labelRad = " + labelRad);

			unlabeledLive = allLive - labeledLive;

			for (int i = cell.length - 1; i >= 0; i--) {
				activitytotal = activitytotal + cell[i][5];
			}

			progress.clearRect(0, 0, jWidth, jHeight);
			progress.setColor(Color.GREEN);
			progress.fillRect(1, 1, jWidth, jHeight);
			progress.setColor(Color.BLACK);
			progress.drawString("Plotting Data - please wait", jWidth / 2 - 50, jHeight / 2 + 6);

			double[][] totalDoses = new double[11][3]; // totalDoses[ICODE][mixed | labeled | unlabeled]
			for (int i = 0; i < 11; i++) {
				for (int j = 0; j < 3; j++) {
					totalDoses[i][j] = 0;
				}
			}
			for (int i = 0; i < cellnumber; i++) {
				for (int ICODE = 1; ICODE < 12; ICODE++) {
					totalDoses[ICODE - 1][0] += crossDoses[i][ICODE - 1]
							+ selfSValues[ICODE][2] * activityFractions[1] * cell[i][5]
							+ selfSValues[ICODE][3] * activityFractions[2] * cell[i][5]
							+ selfSValues[ICODE][4] * activityFractions[3] * cell[i][5];
					if(cell[i][4] != 0) {
						totalDoses[ICODE - 1][1] += crossDoses[i][ICODE - 1]
								+ selfSValues[ICODE][2] * activityFractions[1] * cell[i][5]
								+ selfSValues[ICODE][3] * activityFractions[2] * cell[i][5]
								+ selfSValues[ICODE][4] * activityFractions[3] * cell[i][5];
					}
					else {
						totalDoses[ICODE - 1][2] += crossDoses[i][ICODE - 1];
					}
				}
			}

			for (double k = 1D; k <= step; k++) {
				Arrays.fill(Survival, 1);
				allLive = 0;
				labeledLive = 0;
				unlabeledLive = 0;
				ratio = k / step;

				for (int i = 0; i < cellnumber; i++) {
					for (int ICODE = 0; ICODE < 11; ICODE++) {
						/* self, N->N */
						if (complexRadiobiologicalParams[ICODE][0] != 0 || complexRadiobiologicalParams[ICODE][1] != 0)
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][0] * ratio * selfSValues[ICODE + 1][2] * activityFractions[1] * cell[i][5] - complexRadiobiologicalParams[ICODE][1] * ratio * selfSValues[ICODE + 1][2] * activityFractions[1] * cell[i][5] * ratio * selfSValues[ICODE + 1][2] * activityFractions[1] * cell[i][5]);
						/* self, Cy->N */
						if (complexRadiobiologicalParams[ICODE][2] != 0 || complexRadiobiologicalParams[ICODE][3] != 0)
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][2] * ratio * selfSValues[ICODE + 1][3] * activityFractions[2] * cell[i][5] - complexRadiobiologicalParams[ICODE][3] * ratio * selfSValues[ICODE + 1][3] * activityFractions[2] * cell[i][5] * ratio * selfSValues[ICODE + 1][3] * activityFractions[2] * cell[i][5]);
						/* self, CS->N */
						if (complexRadiobiologicalParams[ICODE][4] != 0 || complexRadiobiologicalParams[ICODE][5] != 0)
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][4] * ratio * selfSValues[ICODE + 1][4] * activityFractions[3] * cell[i][5] - complexRadiobiologicalParams[ICODE][5] * ratio * selfSValues[ICODE + 1][4] * activityFractions[3] * cell[i][5] * ratio * selfSValues[ICODE + 1][4] * activityFractions[3] * cell[i][5]);
						/* cross, N->N & Cy->N & CS->N */
						if (complexRadiobiologicalParams[ICODE][6] != 0 || complexRadiobiologicalParams[ICODE][7] != 0)
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][6] * ratio * crossDoses[i][ICODE] - complexRadiobiologicalParams[ICODE][7] * ratio * crossDoses[i][ICODE] * ratio * crossDoses[i][ICODE]);
					}

					rand = random.nextDouble();
					if (rand < Survival[i] && !coldLiving[i]) {
						allLive++;
						if (cell[i][4] != 0) {
							unlabeledLive++;
						} else {
							labeledLive++;
						}
					}
				}

				tempsurvall = (double) allLive / (double) cellnumber;
				if (labelCell != 0) {
					tempsurvlabel = (double) labeledLive / (double) labelCell;
				}
				if (unlableCell != 0) {
					tempsurvunlabel = (double) unlabeledLive / (double) unlableCell;
				}

				NumberFormat nf = new DecimalFormat("0.00E00");
				output += ""
						+ nf.format( (MAC * ratio) ) + "\t"
						+ nf.format( (MeanABD * ratio) ) + "\t"
						+ nf.format(totalDoses[0][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[1][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[2][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[3][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[4][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[5][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[6][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[7][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[8][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[9][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[10][0] / cellnumber * ratio ) + "\t"
						+ nf.format(MAC * ratio * cellnumber / labelCell) + "\t"
						+ nf.format(MeanABDL * ratio) + "\t"
						+ nf.format(totalDoses[0][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[1][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[2][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[3][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[4][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[5][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[6][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[7][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[8][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[9][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[10][1] / cellnumber * ratio ) + "\t"
						+ nf.format(MeanABDUL * ratio) + "\t"
						+ nf.format(totalDoses[0][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[1][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[2][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[3][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[4][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[5][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[6][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[7][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[8][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[9][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[10][2] / cellnumber * ratio ) + "\t"
						+ nf.format(tempsurvlabel) + "\t"
						+ nf.format(tempsurvunlabel) + "\t"
						+ nf.format(tempsurvall) + "\n";

				PlotOutput[0][PlotPoints] = (MAC * ratio);                          //MAC
				PlotOutput[1][PlotPoints] = (MeanABD * ratio);                      //MDC
				PlotOutput[2][PlotPoints] = (MAC * ratio * cellnumber / labelCell); //MALC
				PlotOutput[3][PlotPoints] = (MeanABDL * ratio);                     //MDLC
				PlotOutput[4][PlotPoints] = (MeanABDUL * ratio);                    //MDULC
				PlotOutput[5][PlotPoints] = (tempsurvlabel);                        //SF of Labeled
				PlotOutput[6][PlotPoints] = (tempsurvunlabel);                      //SF of unlabeled
				PlotOutput[7][PlotPoints] = (tempsurvall);                          //SF of all

				PlotPoints++;
			}
			totalDoses = null; // free up some memory space
		}
		else if (radiationtarget == 2) {
			// radiation target: Cytoplasm.
			for (int i = 0; i < cellnumber; i++) {
			/* for each cell */
				if (cell[i][4] != 0) {
				/* if the cell is labeled calculate self dose exponential and decrease survival rate */
					for (int ICODE = 0; ICODE < 11; ICODE++) {
						// the if statements below are to not do useless work
					/* self, N->Cy */
						if (complexRadiobiologicalParams[ICODE][0] != 0 || complexRadiobiologicalParams[ICODE][1] != 0) {
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][0] * selfSValues[ICODE + 1][5] * activityFractions[1] * cell[i][5] - complexRadiobiologicalParams[ICODE][1] * selfSValues[ICODE + 1][5] * selfSValues[ICODE + 1][5] * activityFractions[1] * activityFractions[1] * cell[i][5] * cell[i][5]);
							totalRad += selfSValues[ICODE + 1][5] * activityFractions[1] * cell[i][5];
							labelRad += selfSValues[ICODE + 1][5] * activityFractions[1] * cell[i][5];
						}
					/* self, Cy->Cy */
						if (complexRadiobiologicalParams[ICODE][2] != 0 || complexRadiobiologicalParams[ICODE][3] != 0) {
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][2] * selfSValues[ICODE + 1][7] * activityFractions[2] * cell[i][5] - complexRadiobiologicalParams[ICODE][3] * selfSValues[ICODE + 1][7] * selfSValues[ICODE + 1][7] * activityFractions[2] * activityFractions[2] * cell[i][5] * cell[i][5]);
							totalRad += selfSValues[ICODE + 1][7] * activityFractions[2] * cell[i][5];
							labelRad += selfSValues[ICODE + 1][7] * activityFractions[2] * cell[i][5];
						}
					/* self, CS->Cy */
						if (complexRadiobiologicalParams[ICODE][4] != 0 || complexRadiobiologicalParams[ICODE][5] != 0) {
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][4] * selfSValues[ICODE + 1][6] * activityFractions[3] * cell[i][5] - complexRadiobiologicalParams[ICODE][5] * selfSValues[ICODE + 1][6] * selfSValues[ICODE + 1][6] * activityFractions[3] * activityFractions[3] * cell[i][5] * cell[i][5]);
							totalRad += selfSValues[ICODE + 1][6] * activityFractions[3] * cell[i][5];
							labelRad += selfSValues[ICODE + 1][6] * activityFractions[3] * cell[i][5];
						}
					}
				}

				for (int ICODE = 0; ICODE < 11; ICODE++) {
					for (int j = 0; j < labelCell; j++) {
						/* dose that cell from each labled cell */
						x = celllabel[j][1] - cell[i][1];
						y = celllabel[j][2] - cell[i][2];
						z = celllabel[j][3] - cell[i][3];
						dist = Math.sqrt(x * x + y * y + z * z);
						if(dist == 0){
							continue;
						}

						if (dist > SValues[0].length - 1) {
							dist = SValues[0].length - 1;
						}

						crossDoses[i][ICODE] += SValues[ICODE + 1][(int)dist - 2 * rCell][5] * celllabel[j][5] * activityFractions[1]; /* cross, N->Cy */
						crossDoses[i][ICODE] += SValues[ICODE + 1][(int)dist - 2 * rCell][7] * celllabel[j][5] * activityFractions[2]; /* cross, Cy->Cy */
						crossDoses[i][ICODE] += SValues[ICODE + 1][(int)dist - 2 * rCell][6] * celllabel[j][5] * activityFractions[3]; /* cross, CS->Cy */
						cell[i][7] += crossDoses[i][ICODE];
					}

					if (complexRadiobiologicalParams[ICODE][6] != 0 || complexRadiobiologicalParams[ICODE][7] != 0) {
						Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][6] * crossDoses[i][ICODE] - complexRadiobiologicalParams[ICODE][7] * crossDoses[i][ICODE] * crossDoses[i][ICODE]);
						totalRad += crossDoses[i][ICODE];
						if (cell[i][4] != 0) {
							labelRad += crossDoses[i][ICODE];
						}
					}
				}

				rand = random.nextDouble();
				if (rand >= Survival[i] || coldLiving[i]) {
					cell[i][0] = 0;
				} else {
					allLive++;
					if (cell[i][4] != 0) {
						labeledLive++;
					}
				}

				// update the progress bar
				percent = ((double) i / (double) cellnumber) * (double) jWidth;
				progress.clearRect(0, 0, jWidth, jHeight);
				progress.setColor(Color.GREEN);
				progress.fillRect(1, 1, (int) percent, 28);
				progress.setColor(Color.BLACK);
				progress.drawString("Part 2 of 2 : " + String.format("%1$.2f", percent / jWidth * 100) + "%", jWidth / 2 - 50, jHeight / 2 + 6);
			}

			MeanABD = totalRad / cellnumber;
			MeanABDL = labelRad / labelCell;
			if ((cellnumber - labelCell) != 0) {
				MeanABDUL = (totalRad - labelRad) / (cellnumber - labelCell);
			}
			System.err.println("MeanABDUL = " + MeanABDUL);
			System.err.println("totalRad = " + totalRad);
			System.err.println("labelRad = " + labelRad);

			unlabeledLive = allLive - labeledLive;

			for (int i = cell.length - 1; i >= 0; i--) {
				activitytotal = activitytotal + cell[i][5];
			}

			progress.clearRect(0, 0, jWidth, jHeight);
			progress.setColor(Color.GREEN);
			progress.fillRect(1, 1, jWidth, jHeight);
			progress.setColor(Color.BLACK);
			progress.drawString("Plotting Data - please wait", jWidth / 2 - 50, jHeight / 2 + 6);

			double[][] totalDoses = new double[11][3]; // totalDoses[ICODE][mixed | labeled | unlabeled]
			for (int i = 0; i < cellnumber; i++) {
				for (int ICODE = 1; ICODE < 12; ICODE++) {
					totalDoses[ICODE - 1][0] += crossDoses[i][ICODE - 1]
							+ selfSValues[ICODE][5] * activityFractions[1] * cell[i][5]
							+ selfSValues[ICODE][7] * activityFractions[2] * cell[i][5]
							+ selfSValues[ICODE][6] * activityFractions[3] * cell[i][5];
					if(cell[i][4] != 0) {
						totalDoses[ICODE - 1][1] += crossDoses[i][ICODE - 1]
								+ selfSValues[ICODE][5] * activityFractions[1] * cell[i][5]
								+ selfSValues[ICODE][7] * activityFractions[2] * cell[i][5]
								+ selfSValues[ICODE][6] * activityFractions[3] * cell[i][5];
					}
					else {
						totalDoses[ICODE - 1][2] += crossDoses[i][ICODE - 1];
					}
				}
			}

			for (double k = 1D; k <= step; k++) {
				Arrays.fill(Survival, 1);
				allLive = 0;
				labeledLive = 0;
				unlabeledLive = 0;
				ratio = k / step;

				for (int i = 0; i < cellnumber; i++) {
					for (int ICODE = 0; ICODE < 11; ICODE++) {
						/* self, N->Cy */
						if (complexRadiobiologicalParams[ICODE][0] != 0 || complexRadiobiologicalParams[ICODE][1] != 0)
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][0] * activityFractions[1] * ratio * selfSValues[ICODE + 1][5] * cell[i][5] - complexRadiobiologicalParams[ICODE][1] * ratio * selfSValues[ICODE + 1][5] * cell[i][5] * activityFractions[1] * ratio * selfSValues[ICODE + 1][5] * cell[i][5] * activityFractions[1]);
						/* self, Cy->Cy */
						if (complexRadiobiologicalParams[ICODE][2] != 0 || complexRadiobiologicalParams[ICODE][3] != 0)
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][2] * activityFractions[2] * ratio * selfSValues[ICODE + 1][7] * cell[i][5] - complexRadiobiologicalParams[ICODE][3] * ratio * selfSValues[ICODE + 1][7] * cell[i][5] * activityFractions[2] * ratio * selfSValues[ICODE + 1][7] * cell[i][5] * activityFractions[2]);
						/* self, Cs->Cy */
						if (complexRadiobiologicalParams[ICODE][4] != 0 || complexRadiobiologicalParams[ICODE][5] != 0)
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][4] * activityFractions[3] * ratio * selfSValues[ICODE + 1][6] * cell[i][5] - complexRadiobiologicalParams[ICODE][5] * ratio * selfSValues[ICODE + 1][6] * cell[i][5] * activityFractions[3] * ratio * selfSValues[ICODE + 1][6] * cell[i][5] * activityFractions[3]);
						/* cross, N->Cy & Cy->Cy & CS->Cy */
						if (complexRadiobiologicalParams[ICODE][6] != 0 || complexRadiobiologicalParams[ICODE][7] != 0)
							Survival[i] *= Math.exp(-1D * crossDoses[i][ICODE] * ratio * complexRadiobiologicalParams[ICODE][6] - complexRadiobiologicalParams[ICODE][11] * crossDoses[i][ICODE] * ratio * crossDoses[i][ICODE] * ratio);
					}

					rand = random.nextDouble();
					if (rand < Survival[i] && !coldLiving[i]) {
						allLive++;
						if (cell[i][4] == 0) {
							unlabeledLive++;
						} else {
							labeledLive++;
						}
					}
				}

				tempsurvall = (double) allLive / (double) cellnumber;
				if (labelCell != 0) {
					tempsurvlabel = (double) labeledLive / (double) labelCell;
				}
				if (unlableCell != 0) {
					tempsurvunlabel = (double) unlabeledLive / (double) unlableCell;
				}

				NumberFormat nf = new DecimalFormat("0.00E00");
				output += ""
						+ nf.format( (MAC * ratio) ) + "\t"
						+ nf.format( (MeanABD * ratio) ) + "\t"
						+ nf.format(totalDoses[0][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[1][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[2][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[3][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[4][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[5][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[6][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[7][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[8][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[9][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[10][0] / cellnumber * ratio ) + "\t"
						+ nf.format(MAC * ratio * cellnumber / labelCell) + "\t"
						+ nf.format(MeanABDL * ratio) + "\t"
						+ nf.format(totalDoses[0][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[1][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[2][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[3][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[4][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[5][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[6][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[7][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[8][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[9][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[10][1] / cellnumber * ratio ) + "\t"
						+ nf.format(MeanABDUL * ratio) + "\t"
						+ nf.format(totalDoses[0][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[1][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[2][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[3][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[4][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[5][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[6][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[7][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[8][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[9][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[10][2] / cellnumber * ratio ) + "\t"
						+ nf.format(tempsurvlabel) + "\t"
						+ nf.format(tempsurvunlabel) + "\t"
						+ nf.format(tempsurvall) + "\n";

				PlotOutput[0][PlotPoints] = (MAC * ratio);                          //MAC
				PlotOutput[1][PlotPoints] = (MeanABD * ratio);                      //MDC
				PlotOutput[2][PlotPoints] = (MAC * ratio * cellnumber / labelCell); //MALC
				PlotOutput[3][PlotPoints] = (MeanABDL * ratio);                     //MDLC
				PlotOutput[4][PlotPoints] = (MeanABDUL * ratio);                    //MDULC
				PlotOutput[5][PlotPoints] = tempsurvlabel;                          //SF of Labeled
				PlotOutput[6][PlotPoints] = tempsurvunlabel;                        //SF of unlabeled
				PlotOutput[7][PlotPoints] = tempsurvall;                            //SF of all

				PlotPoints++;
			}
			totalDoses = null; // free up some memory space
		}
		else if (radiationtarget == 3) {
			// radiation target: Nucleus & Cytoplasm.

			/* This section needs a second crossdoses array[][] to deal with the dosing of 2 separate targets*/
			double crossDoses2[][] = new double[cellnumber][11];
			double totalRad2 = 0;
			double labelRad2 = 0;

			for (int i = 0; i < cellnumber; i++) {
			/* for each cell */
				if (cell[i][4] != 0) {
				/* if the cell is labeled calculate self dose exponential and decrease survival rate */
					for (int ICODE = 0; ICODE < 11; ICODE++) {
						// the if statements below are to not do useless work
						/* self, N->N */
						if (complexRadiobiologicalParams[ICODE][0] != 0 || complexRadiobiologicalParams[ICODE][1] != 0) {
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][0] * selfSValues[ICODE + 1][2] * activityFractions[1] * cell[i][5] - complexRadiobiologicalParams[ICODE][1] * selfSValues[ICODE + 1][2] * selfSValues[ICODE + 1][2] * activityFractions[1] * activityFractions[1] * cell[i][5] * cell[i][5]);
							totalRad += selfSValues[ICODE + 1][2] * activityFractions[1] * cell[i][5];
							labelRad += selfSValues[ICODE + 1][2] * activityFractions[1] * cell[i][5];
						}
						/* self, Cy->N */
						if (complexRadiobiologicalParams[ICODE][2] != 0 || complexRadiobiologicalParams[ICODE][3] != 0) {
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][2] * selfSValues[ICODE + 1][3] * activityFractions[2] * cell[i][5] - complexRadiobiologicalParams[ICODE][3] * selfSValues[ICODE + 1][3] * selfSValues[ICODE + 1][3] * activityFractions[2] * activityFractions[2] * cell[i][5] * cell[i][5]);
							totalRad += selfSValues[ICODE + 1][3] * activityFractions[2] * cell[i][5];
							labelRad += selfSValues[ICODE + 1][3] * activityFractions[2] * cell[i][5];
						}
						/* self, CS->N */
						if (complexRadiobiologicalParams[ICODE][4] != 0 || complexRadiobiologicalParams[ICODE][5] != 0) {
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][4] * selfSValues[ICODE + 1][4] * activityFractions[3] * cell[i][5] - complexRadiobiologicalParams[ICODE][5] * selfSValues[ICODE + 1][4] * selfSValues[ICODE + 1][4] * activityFractions[3] * activityFractions[3] * cell[i][5] * cell[i][5]);
							totalRad += selfSValues[ICODE + 1][4] * activityFractions[3] * cell[i][5];
							labelRad += selfSValues[ICODE + 1][4] * activityFractions[3] * cell[i][5];
						}
						/* self, N->Cy */
						if (complexRadiobiologicalParams[ICODE][6] != 0 || complexRadiobiologicalParams[ICODE][7] != 0) {
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][6] * selfSValues[ICODE + 1][5] * activityFractions[1] * cell[i][5] - complexRadiobiologicalParams[ICODE][7] * selfSValues[ICODE + 1][5] * selfSValues[ICODE + 1][5] * activityFractions[1] * activityFractions[1] * cell[i][5] * cell[i][5]);
							totalRad += selfSValues[ICODE + 1][5] * activityFractions[1] * cell[i][5];
							labelRad += selfSValues[ICODE + 1][5] * activityFractions[1] * cell[i][5];
						}
						/* self, Cy->Cy */
						if (complexRadiobiologicalParams[ICODE][8] != 0 || complexRadiobiologicalParams[ICODE][9] != 0) {
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][8] * selfSValues[ICODE + 1][7] * activityFractions[2] * cell[i][5] - complexRadiobiologicalParams[ICODE][9] * selfSValues[ICODE + 1][7] * selfSValues[ICODE + 1][7] * activityFractions[2] * activityFractions[2] * cell[i][5] * cell[i][5]);
							totalRad += selfSValues[ICODE + 1][6] * activityFractions[2] * cell[i][5];
							labelRad += selfSValues[ICODE + 1][6] * activityFractions[2] * cell[i][5];
						}
						/* self, CS->Cy */
						if (complexRadiobiologicalParams[ICODE][10] != 0 || complexRadiobiologicalParams[ICODE][11] != 0) {
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][10] * selfSValues[ICODE + 1][6] * activityFractions[3] * cell[i][5] - complexRadiobiologicalParams[ICODE][11] * selfSValues[ICODE + 1][6] * selfSValues[ICODE + 1][6] * activityFractions[3] * activityFractions[3] * cell[i][5] * cell[i][5]);
							totalRad += selfSValues[ICODE + 1][7] * activityFractions[3] * cell[i][5];
							labelRad += selfSValues[ICODE + 1][7] * activityFractions[3] * cell[i][5];
						}
					}
				}

				for (int ICODE = 0; ICODE < 11; ICODE++) {
					for (int j = 0; j < labelCell; j++) {
						/* dose that cell from each labled cell */
						x = celllabel[j][1] - cell[i][1];
						y = celllabel[j][2] - cell[i][2];
						z = celllabel[j][3] - cell[i][3];
						dist = Math.sqrt(x * x + y * y + z * z);
						if(dist == 0){
							continue;
						}

						if (dist > SValues[0].length - 1) {
							dist = SValues[0].length - 1;
						}

						crossDoses[i][ICODE] += SValues[ICODE + 1][(int)dist - 2 * rCell][2] * celllabel[j][5] * activityFractions[1]; /* cross, N->N */
						crossDoses[i][ICODE] += SValues[ICODE + 1][(int)dist - 2 * rCell][3] * celllabel[j][5] * activityFractions[2]; /* cross, Cy->N */
						crossDoses[i][ICODE] += SValues[ICODE + 1][(int)dist - 2 * rCell][4] * celllabel[j][5] * activityFractions[3]; /* cross, CS->N */

						crossDoses2[i][ICODE] += SValues[ICODE + 1][(int)dist - 2 * rCell][5] * celllabel[j][5] * activityFractions[1]; /* cross, N->Cy */
						crossDoses2[i][ICODE] += SValues[ICODE + 1][(int)dist - 2 * rCell][7] * celllabel[j][5] * activityFractions[2]; /* cross, Cy->Cy */
						crossDoses2[i][ICODE] += SValues[ICODE + 1][(int)dist - 2 * rCell][6] * celllabel[j][5] * activityFractions[3]; /* cross, CS->Cy */
						cell[i][7] += crossDoses[i][ICODE];
						cell[i][7] += crossDoses2[i][ICODE];
					}

					if (complexRadiobiologicalParams[ICODE][12] != 0 || complexRadiobiologicalParams[ICODE][13] != 0) {
						Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][12] * crossDoses[i][ICODE] - complexRadiobiologicalParams[ICODE][13] * crossDoses[i][ICODE] * crossDoses[i][ICODE]);
						totalRad += crossDoses[i][ICODE];
						if (cell[i][4] != 0) {
							labelRad += crossDoses[i][ICODE];
						}
					}
					if (complexRadiobiologicalParams[ICODE][14] != 0 || complexRadiobiologicalParams[ICODE][15] != 0) {
						Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][14] * crossDoses2[i][ICODE] - complexRadiobiologicalParams[ICODE][15] * crossDoses2[i][ICODE] * crossDoses2[i][ICODE]);
						totalRad2 += crossDoses2[i][ICODE];
						if (cell[i][4] != 0) {
							labelRad2 += crossDoses2[i][ICODE];
						}
					}
				}

				rand = random.nextDouble();
				if (rand >= Survival[i] || coldLiving[i]) {
					cell[i][0] = 0;
				} else {
					allLive++;
					if (cell[i][4] != 0) {
						labeledLive++;
					}
				}

				// update the progress bar
				percent = ((double) i / (double) cellnumber) * (double) jWidth;
				progress.clearRect(0, 0, jWidth, jHeight);
				progress.setColor(Color.GREEN);
				progress.fillRect(1, 1, (int) percent, 28);
				progress.setColor(Color.BLACK);
				progress.drawString("Part 2 of 2 : " + String.format("%1$.2f", percent / jWidth * 100) + "%", jWidth / 2 - 50, jHeight / 2 + 6);
			}

			MeanABD = totalRad / cellnumber;
			double MeanABD2 = totalRad2 / cellnumber;
			MeanABDL = labelRad / labelCell;
			double MeanABDL2 = labelRad2 / labelCell;
			double MeanABDUL2 = 0;
			if ((cellnumber - labelCell) != 0) {
				MeanABDUL = (totalRad - labelRad) / (cellnumber - labelCell);
				MeanABDUL2 = (totalRad2 - labelRad2) / (cellnumber - labelCell);
			}
			System.err.println("MeanABDUL = " + MeanABDUL);
			System.err.println("totalRad = " + totalRad);
			System.err.println("labelRad = " + labelRad);

			unlabeledLive = allLive - labeledLive;

			for (int i = cell.length - 1; i >= 0; i--) {
				activitytotal = activitytotal + cell[i][5];
			}

			progress.clearRect(0, 0, jWidth, jHeight);
			progress.setColor(Color.GREEN);
			progress.fillRect(1, 1, jWidth, jHeight);
			progress.setColor(Color.BLACK);
			progress.drawString("Plotting Data - please wait", jWidth / 2 - 50, jHeight / 2 + 6);

			double[][] totalDoses = new double[11][3]; // totalDoses[ICODE][mixed | labeled | unlabeled]
			double[][] totalDoses2 = new double[11][3]; // totalDoses[ICODE][mixed | labeled | unlabeled]
			for (int i = 0; i < cellnumber; i++) {
				for (int ICODE = 1; ICODE < 12; ICODE++) {
					totalDoses[ICODE - 1][0] += crossDoses[i][ICODE - 1]
							+ selfSValues[ICODE][2] * activityFractions[1] * cell[i][5]
							+ selfSValues[ICODE][3] * activityFractions[2] * cell[i][5]
							+ selfSValues[ICODE][4] * activityFractions[3] * cell[i][5];
					totalDoses2[ICODE - 1][0] += crossDoses2[i][ICODE - 1]
							+ selfSValues[ICODE][5] * activityFractions[1] * cell[i][5]
							+ selfSValues[ICODE][7] * activityFractions[2] * cell[i][5]
							+ selfSValues[ICODE][6] * activityFractions[3] * cell[i][5];
					if(cell[i][4] != 0) {
						totalDoses[ICODE - 1][1] += crossDoses[i][ICODE - 1]
								+ selfSValues[ICODE][2] * activityFractions[1] * cell[i][5]
								+ selfSValues[ICODE][3] * activityFractions[2] * cell[i][5]
								+ selfSValues[ICODE][4] * activityFractions[3] * cell[i][5];
						totalDoses2[ICODE - 1][1] += crossDoses2[i][ICODE - 1]
								+ selfSValues[ICODE][5] * activityFractions[1] * cell[i][5]
								+ selfSValues[ICODE][7] * activityFractions[2] * cell[i][5]
								+ selfSValues[ICODE][6] * activityFractions[3] * cell[i][5];					}
					else {
						totalDoses[ICODE - 1][2] += crossDoses[i][ICODE - 1];
						totalDoses2[ICODE - 1][2] += crossDoses2[i][ICODE - 1];
					}
				}
			}

			output = "Doses to the nucleus and cytoplasm are shown seperately when using both as the target region. ";
			output += "Doses to nucleus\n";
			output += "MAC(Bq)\tMDC(all)\tMDC(γ)\tMDC(X)\tMDC(AQ)\tMDC(β+)\tMDC(β-)\tMDC(IE)\tMDC(Auger)\tMDC(α)\tMDC(αR)\tMDC(FF)\tMDC(N)\tMALC(Bq)\tMDLC(all)\tMDLC(γ)\tMDLC(X)\tMDLC(AQ)\tMDLC(β+)\tMDLC(β-)\tMDLC(IE)\tMDLC(Auger)\tMDLC(α)\tMDLC(αR)\tMDLC(FF)\tMDLC(N)\t MDUC(all)\tMDUC(γ)\tMDUC(X)\tMDUC(AQ)\tMDUC(β+)\tMDUC(β-)\tMDUC(IE)\tMDUC(Auger)\tMDUC(α)\tMDUC(αR)\tMDUC(FF)\tMDUC(N)\tSF(labeled)\tSF(unlabeled)\tSF(all cells)" + "\n";
			String output2 = "Doses to cytoplasm\n";
			output2 += "MAC(Bq)\tMDC(all)\tMDC(γ)\tMDC(X)\tMDC(AQ)\tMDC(β+)\tMDC(β-)\tMDC(IE)\tMDC(Auger)\tMDC(α)\tMDC(αR)\tMDC(FF)\tMDC(N)\tMALC(Bq)\tMDLC(all)\tMDLC(γ)\tMDLC(X)\tMDLC(AQ)\tMDLC(β+)\tMDLC(β-)\tMDLC(IE)\tMDLC(Auger)\tMDLC(α)\tMDLC(αR)\tMDLC(FF)\tMDLC(N)\t MDUC(all)\tMDUC(γ)\tMDUC(X)\tMDUC(AQ)\tMDUC(β+)\tMDUC(β-)\tMDUC(IE)\tMDUC(Auger)\tMDUC(α)\tMDUC(αR)\tMDUC(FF)\tMDUC(N)\tSF(labeled)\tSF(unlabeled)\tSF(all cells)" + "\n";

			for (double k = 1D; k <= step; k++) {
				Arrays.fill(Survival, 1);
				allLive = 0;
				labeledLive = 0;
				unlabeledLive = 0;
				ratio = k / step;

				for (int i = 0; i < cellnumber; i++) {
					for (int ICODE = 0; ICODE < 11; ICODE++) {
						/* self, N->N */
						if (complexRadiobiologicalParams[ICODE][0] != 0 || complexRadiobiologicalParams[ICODE][1] != 0)
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][0] * ratio * selfSValues[ICODE + 1][2] * activityFractions[1] * cell[i][5] - complexRadiobiologicalParams[ICODE][1] * ratio * ratio * selfSValues[ICODE + 1][2] * selfSValues[ICODE + 1][2] * activityFractions[1] * activityFractions[1] * cell[i][5] * cell[i][5]);
						/* self, Cy->N */
						if (complexRadiobiologicalParams[ICODE][2] != 0 || complexRadiobiologicalParams[ICODE][3] != 0)
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][2] * ratio * selfSValues[ICODE + 1][3] * activityFractions[2] * cell[i][5] - complexRadiobiologicalParams[ICODE][3] * ratio * ratio * selfSValues[ICODE + 1][3] * selfSValues[ICODE + 1][3] * activityFractions[2] * activityFractions[2] * cell[i][5] * cell[i][5]);
						/* self, CS->N */
						if (complexRadiobiologicalParams[ICODE][4] != 0 || complexRadiobiologicalParams[ICODE][5] != 0)
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][4] * ratio * selfSValues[ICODE + 1][4] * activityFractions[3] * cell[i][5] - complexRadiobiologicalParams[ICODE][5] * ratio * ratio * selfSValues[ICODE + 1][4] * selfSValues[ICODE + 1][4] * activityFractions[3] * activityFractions[3] * cell[i][5] * cell[i][5]);
						/* self, N->Cy */
						if (complexRadiobiologicalParams[ICODE][6] != 0 || complexRadiobiologicalParams[ICODE][7] != 0)
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][6] * ratio * selfSValues[ICODE + 1][5] * activityFractions[1] * cell[i][5] - complexRadiobiologicalParams[ICODE][7] * ratio * ratio * selfSValues[ICODE + 1][5] * selfSValues[ICODE + 1][5] * activityFractions[1] * activityFractions[1] * cell[i][5] * cell[i][5]);
						/* self, Cy->Cy */
						if (complexRadiobiologicalParams[ICODE][8] != 0 || complexRadiobiologicalParams[ICODE][9] != 0)
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][8] * ratio * selfSValues[ICODE + 1][7] * activityFractions[2] * cell[i][5] - complexRadiobiologicalParams[ICODE][9] * ratio * ratio * selfSValues[ICODE + 1][7] * selfSValues[ICODE + 1][7] * activityFractions[2] * activityFractions[2] * cell[i][5] * cell[i][5]);
						/* self, CS->Cy */
						if (complexRadiobiologicalParams[ICODE][10] != 0 || complexRadiobiologicalParams[ICODE][11] != 0)
							Survival[i] *= Math.exp(-1D * complexRadiobiologicalParams[ICODE][10] * ratio * selfSValues[ICODE + 1][6] * activityFractions[3] * cell[i][5] - complexRadiobiologicalParams[ICODE][11] * ratio * ratio * selfSValues[ICODE + 1][6] * selfSValues[ICODE + 1][6] * activityFractions[3] * activityFractions[3] * cell[i][5] * cell[i][5]);
						/* cross, N->N & Cy->N & CS->N */
						if (complexRadiobiologicalParams[ICODE][12] != 0 || complexRadiobiologicalParams[ICODE][13] != 0)
							Survival[i] *= Math.exp(-1D * crossDoses[i][ICODE] * ratio * complexRadiobiologicalParams[ICODE][12] - complexRadiobiologicalParams[ICODE][13] * crossDoses[i][ICODE] * crossDoses[i][ICODE] * ratio * ratio);
						/* cross, N->Cy & Cy->Cy & CS->Cy */
						if (complexRadiobiologicalParams[ICODE][14] != 0 || complexRadiobiologicalParams[ICODE][15] != 0)
							Survival[i] *= Math.exp(-1D * crossDoses2[i][ICODE] * ratio * complexRadiobiologicalParams[ICODE][14] - complexRadiobiologicalParams[ICODE][15] * crossDoses2[i][ICODE] * crossDoses2[i][ICODE] * ratio * ratio);
					}

					rand = random.nextDouble();
					if (rand < Survival[i] && !coldLiving[i]) {
						allLive++;
						if (cell[i][4] == 0) {
							unlabeledLive++;
						} else {
							labeledLive++;
						}
					}
				}

				tempsurvall = (double) allLive / (double) cellnumber;
				if (labelCell != 0) {
					tempsurvlabel = (double) labeledLive / (double) labelCell;
				}
				if (unlableCell != 0) {
					tempsurvunlabel = (double) unlabeledLive / (double) unlableCell;
				}

				NumberFormat nf = new DecimalFormat("0.00E00");
				output += ""
						+ nf.format( (MAC * ratio) ) + "\t"
						+ nf.format( (MeanABD * ratio) ) + "\t"
						+ nf.format(totalDoses[0][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[1][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[2][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[3][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[4][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[5][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[6][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[7][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[8][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[9][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[10][0] / cellnumber * ratio ) + "\t"
						+ nf.format(MAC * ratio * cellnumber / labelCell) + "\t"
						+ nf.format(MeanABDL * ratio) + "\t"
						+ nf.format(totalDoses[0][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[1][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[2][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[3][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[4][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[5][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[6][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[7][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[8][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[9][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[10][1] / cellnumber * ratio ) + "\t"
						+ nf.format(MeanABDUL * ratio) + "\t"
						+ nf.format(totalDoses[0][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[1][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[2][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[3][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[4][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[5][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[6][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[7][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[8][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[9][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses[10][2] / cellnumber * ratio ) + "\t"
						+ nf.format(tempsurvlabel) + "\t"
						+ nf.format(tempsurvunlabel) + "\t"
						+ nf.format(tempsurvall) + "\n";

				output2 += ""
						+ nf.format( (MAC * ratio) ) + "\t"
						+ nf.format( (MeanABD2 * ratio) ) + "\t"
						+ nf.format(totalDoses2[0][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[1][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[2][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[3][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[4][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[5][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[6][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[7][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[8][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[9][0] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[10][0] / cellnumber * ratio ) + "\t"
						+ nf.format(MAC * ratio * cellnumber / labelCell) + "\t"
						+ nf.format(MeanABDL2 * ratio) + "\t"
						+ nf.format(totalDoses2[0][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[1][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[2][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[3][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[4][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[5][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[6][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[7][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[8][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[9][1] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[10][1] / cellnumber * ratio ) + "\t"
						+ nf.format(MeanABDUL2 * ratio) + "\t"
						+ nf.format(totalDoses2[0][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[1][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[2][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[3][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[4][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[5][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[6][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[7][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[8][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[9][2] / cellnumber * ratio ) + "\t"
						+ nf.format(totalDoses2[10][2] / cellnumber * ratio ) + "\t"
						+ nf.format(tempsurvlabel) + "\t"
						+ nf.format(tempsurvunlabel) + "\t"
						+ nf.format(tempsurvall) + "\n";


				PlotOutput[0][PlotPoints] = (MAC * ratio);                          //MAC
				PlotOutput[1][PlotPoints] = (MeanABD * ratio);                      //MDC
				PlotOutput[2][PlotPoints] = (MAC * ratio * cellnumber / labelCell); //MALC
				PlotOutput[3][PlotPoints] = (MeanABDL * ratio);                     //MDLC
				PlotOutput[4][PlotPoints] = (MeanABDUL * ratio);                    //MDULC
				PlotOutput[5][PlotPoints] = (tempsurvlabel);                        //SF of Labeled
				PlotOutput[6][PlotPoints] = (tempsurvunlabel);                      //SF of unlabeled
				PlotOutput[7][PlotPoints] = (tempsurvall);                          //SF of all

				PlotPoints++;
			}
			totalDoses = null; // free up some memory space
			output += "\n\n" + output2;
			output2 = null;
		}

		PlotOutput[7][0] = 1;
		PlotOutput[6][0] = 1;
		PlotOutput[5][0] = 1;
		return cell;
	}
}
