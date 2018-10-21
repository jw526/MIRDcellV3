package Future;

/**
 * @brief A class to define all the needed enums for this project
 */
public class ENUMS {
	/**
	 * @brief the regions from _ to respectively where the radiation is going
	 */
	public static enum REGION{
		CELL_TO_CELL,
		CELL_SURFACE_TO_CELL,
		NUCLEUS_TO_NUCLEUS,
		CYTOPLASM_TO_NUCLEUS,
		CELL_SURFACE_TO_NUCLEUS,
		NUCLEUS_TO_CYTOPLASM,
		CELL_SURFACE_TO_CYTOPLASM,
		//there is one more but i dont remember what the it was
		OTHER
	}

	/**
	 * @brief the list of icodes as provided by dr howell from the MIRDCell manual
	 */
	public static enum ICODES{
		UNKNOWN,    /**< this is set for any that is unused, unknown, or user defined
		             * it is first to that the internal integer number lines up
		             * with the number given the MIRECell manual */
		GAMMA_RAY,
		X_RAY,
		ANNIHILATION_QUANTA,
		BETA_POS,
		BETA_NEG,
		INTERNAL_CONVERSION,
		AUGER,
		APLHA,
		DAUGHTER_RECOIL,
		FISSION_FRAGMENT,
		NEUTRONS
	}



}
