/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.exception;

public class AboraRuntimeException extends RuntimeException {

	private static final long serialVersionUID = -7665468138537610579L;
	
	public static final String ALREADY_DESTROYED = "AlreadyDestroyed";
	public static final String ALREADY_EXTINCT = "AlreadyExtinct";
	public static final String ALREADY_IN_SET = "AlreadyInSet";
	public static final String ALREADY_IN_TABLE = "AlreadyInTable";
	public static final String AMBIGUOUS_REPLACEMENT = "AmbiguousReplacement";
	public static final String BAD_PRECISION = "BadPrecision";
	public static final String BAD_REQUEST = "BadRequest";
	public static final String BAD_RECIPE = "BadRecipe";
	public static final String BEGINNING_OF_PACKET = "BeginningOfPacket";
	public static final String BOGUS_PROTOCOL = "BogusProtocol";
	public static final String BOGUS_START_INDEX = "BogusStartIndex";
	public static final String CANNOT = "Cannot";
	public static final String CANNOT_U_HAVE_U_MULTIPLE_U_SERVER_U_LOOPS = "CannotHaveMultipleServerLoops";
	public static final String CANNOT_WRAP = "CannotWrap";
	public static final String CANT_CONVERT = "CantConvert";
	public static final String CANT_INFORM = "CantInform";
	public static final String CANT_MAKE_IDENTICAL = "CantMakeIdentical";
	public static final String CANT_MIX_COORDINATE_SPACES = "CantMixCoordinateSpaces";
	public static final String CAST_FAILED = "CastFailed";
	public static final String CLUB_ALREADY_NAMED = "ClubAlreadyNamed";
	public static final String CLUB_NAME_IN_USE = "ClubNameInUse";
	public static final String CLUB_MUST_HAVE_UNIQUE_ID = "ClubMustHaveUniqueId";
	public static final String COMBINE_LOOP_FAILED = "CombineLoopFailed";
	public static final String COPY_OUT_OF_BOUNDS = "CopyOutOfBounds";
	public static final String DESTRUCTED_ABE = "DestructedAbe";
	public static final String DISK_FULL = "DiskFull";
	public static final String DOES_NOT_HAVE_AN_ID = "DoesNotHaveAnId";
	public static final String DOES_NOT_MATCH = "DoesNotMatch";
	public static final String EDIT_CLUB_IRREVOCABLY_REMOVED = "EditClubIrrevocablyRemoved";
	public static final String EDITIONS_REQUIRE_LABELS = "EditionsRequireLabels";
	public static final String EDITOR_REMOVED = "EditorRemoved";
	public static final String EMPTY_REGION = "EmptyRegion";
	public static final String EMPTY_STEPPER = "EmptyStepper";
	public static final String EMPTY_TRAIL = "EmptyTrail";
	public static final String ENDORSEMENT_MUST_BE_FINITE = "EndorsementMustBeFinite";
	public static final String FALLEN_BEHIND_IN_NODE_DOUBLING = "FallenBehindInNodeDoubling";
	public static final String FATAL_ERROR = "FatalError";
	public static final String FOSSIL_EXTINCT = "FossilExtinct";
	public static final String HAS_MULTIPLE_IDS = "HasMultipleIds";
	public static final String HASH_MUST_NOT_CHANGE = "HashMustNotChange";
	public static final String IDALREADY_ASSIGNED = "IDALREADY_ASSIGNED";
	public static final String IDALREADY_USED = "IDALREADY_USED";
	public static final String ILLEGAL_ID = "IllegalId";
	public static final String INCOMPARABLE_TYPE = "INCOMPARABLE_TYPE";
	public static final String INCOMPLETE_ABSTRACTION = "IncompleteAbstraction";
	public static final String INCORRECT_FLOCK_INFO = "IncorrectFlockInfo";
	public static final String INCORRECT_LABEL = "IncorrectLabel";
	public static final String INDEX_OUT_OF_BOUNDS = "IndexOutOfBounds";
	public static final String INTERSECTING_COMBINE = "InterestingCombine";
	public static final String INTRODUCE_FAILED = "IntroduceFailed";
	public static final String INVALID_PARAMETER = "InvalidParameter";
	public static final String INVALID_REQUEST = "InvalidRequest";
	public static final String INVALID_TEXT_POSITION = "InvalidTextPosition";
	public static final String INVALID_TRAIL = "InvalidTrail";
	public static final String MODIFY_BLOCKED_BY_OUTSTANDING_STEPPER = "ModifyBlockedByOutstandingStepper";
	public static final String MODULO_FAILED = "ModuloFailed";
	public static final String MULTIPLE_IMAGES = "MultipleImages";
	public static final String MULTIPLE_PRE_IMAGES = "MultiplePreImages";
	public static final String MUST_BE_AFLOCK = "MustBeAFlock";
	public static final String MUST_BE_ASTUB = "MustBeAStub";
	public static final String MUST_BE_CLUB_DESCRIPTION = "MustBeClubDescription";
	public static final String MUST_BE_CONCRETE_WRAPPER_SPEC = "MustBeConcreteWrapperSpec";
	public static final String MUST_BE_FINITE = "MustBeFinite";
	public static final String MUST_BE_FROZEN = "MustBeFrozen";
	public static final String MUST_BE_INSIDE_TRANSACTION = "MustBeInsideTransaction";
	public static final String MUST_BE_OWNER = "MustBeOwner";
	public static final String MUST_BE_REAL_DISK_MANAGER = "MustBeRealDiskManager";
	public static final String MUST_BE_SIMPLE = "MustBeSimple";
	public static final String MUST_BE_VALID_LOCK_SMITH = "MustBeValidLockSmith";
	public static final String MUST_DO_NEW_SHEPHERD_AFTER_DISK_UPDATE = "MustDoNewShepherdAfterDiskUpdate";
	public static final String MUST_HAVE_ADMIN_AUTHORITY = "MustHaveAdminAuthority";
	public static final String MUST_HAVE_AUTHOR_SIGNATURE_AUTHORITY = "MustHaveAuthorSignatureAuthority";
	public static final String MUST_HAVE_BEEN_GRANTED_AUTHORITY = "MustHaveBeenGrantedAuthority";
	public static final String MUST_HAVE_EDIT_AUTHORITY = "MustHaveEditAuthority";
	public static final String MUST_HAVE_EDIT_PERMISSION = "MustHaveEditPermission";
	public static final String MUST_HAVE_ONE_VALUE = "MustHaveOneValue";
	public static final String MUST_HAVE_READ_PERMISSION = "MustHaveReadPermission";
	public static final String MUST_HAVE_ROOM = "MustHaveRoom";
	public static final String MUST_HAVE_SIGNATURE_AUTHORITY = "MustHaveSignatureAuthority";
	public static final String MUST_HAVE_SINGLE_ELEMENT = "MustHaveSingleElement";
	public static final String MUST_HAVE_SPONSOR_AUTHORITY = "MustHaveSponsorAuthority";
	public static final String MUST_MATCH = "MustMatch";
	public static final String MUST_NOT_BE_NULL = "MustNotBeNull";
	public static final String MUST_NOT_CHANGE_DURING_COMMIT = "MustNotChangeDuringCommit";
	public static final String MUST_NOT_RECORD_CHANGES_FOR_PUMPKINS = "MustNotRecordChangesForPumpkins";
	public static final String MUST_SUPPLY_SOME_HYPER_REF_INFORMATION = "MustSupplySomeHyperRefInformation";
	public static final String MUST_USE_DIFFERENT_LINK_END_KEY = "MustUseDifferentLinkEndKey";
	public static final String MUST_USE_DIFFERENT_LINK_END_NAME = "MustUseDifferentLinkEndName";
	public static final String NEVER_ADDED_DETECTOR = "NeverAddedDetector";
	public static final String NEVER_ADDED_FE_RANGE_ELEMENT = "NeverAddedFeRangeElement";
	public static final String NEVER_ADDED_REVISION_WATCHER = "NeverAddedRevisionWatcher";
	public static final String NEVER_ADDED_WATCHER = "NeverAddedWatcher";
	public static final String NEVER_REGISTERED_KEY_MASTER = "NeverRegisteredKeyMaster";
	public static final String NEW_SHEPHERD_MUST_NOT_HAVE_INFO = "NewShepherdMustNotHaveInfo";
	public static final String NO_AUTHOR = "NoAuthor";
	public static final String NO_BIT_COUNT_LIMIT = "NoBitCountLimit";
	public static final String NO_BOOT_PLAN = "NoBootPlan";
	public static final String NO_FULL_ORDER = "NoFullOrder";
	public static final String NO_HISTORY_CLUB = "NoHistoryClub";
	public static final String NO_HOLE = "NoHole";
	public static final String NO_INNER_WRAPPER = "NoInnerWrapper";
	public static final String NO_LABEL = "NoLabel";
	public static final String NO_PRIVATE_KEY = "NoPrivateKey";
	public static final String NO_PUBLIC_KEY = "NoPublicKey";
	public static final String NO_SUCH_CLUB = "NoSuchClub";
	public static final String NO_SNARF_INFO = "NoSnarfInfo";
	public static final String NON_SHEPHERD = "NonShepherd";
	public static final String NOT_BECOMABLE = "NotBecomable";
	public static final String NOT_CORRECTLY_SIGNED = "NotCorrectlySigned";
	public static final String NOT_ENUMERABLE = "NotEnumerable";
	public static final String NOT_FOUND = "NotFound";
	public static final String NOT_GRABBED = "NotGrabbed";
	public static final String NOT_IN_DOMAIN = "NotInDomain";
	public static final String NOT_IN_LIST = "NotInList";
	public static final String NOT_IN_RANGE = "NotInRange";
	public static final String NOT_IN_SET = "NotInSet";
	public static final String NOT_IN_SPACE = "NotInSpace";
	public static final String NOT_IN_TABLE = "NotInTable";
	public static final String NOT_LOGGED_IN = "NotLoggedIn";
	public static final String NOT_ONE_ELEMENT = "NotOneElement";
	public static final String NOT_SIMPLE = "NotSimple";
	public static final String NOT_SIMPLE_ENOUGH = "NotSimpleEnough";
	public static final String NOT_YET_IMPLEMENTED = "NotYetImplemented";
	public static final String NULL_INSERTION = "NullInsertion";
	public static final String NULL_SHEPHERD = "NullShepherd";
	public static final String NULL_VALUE = "NullValue";
	public static final String NULLFLUID = "NullFluid";
	public static final String OBSOLETE_USAGE_MUST_USE_FETCH_LEFT = "ObsoleteUsageMustUseFetchLeft";
	public static final String OBSOLETE_USAGE_MUST_USE_FETCH_RIGHT = "ObsoleteUsageMustUseFetchRight";
	public static final String OBSOLETE_USAGE_MUST_USE_PAIR_WITH_NULLS = "ObsoleteUsageMustUsePairWithNulls";
	public static final String ONLY_REMOVE_UNCHANGED_FLOCKS = "OnlyRemoveUnchangedFlocks";
	public static final String ORIGINAL_CONTEXT_MUST_BE_FROZEN = "OriginalContextMustBeFrozen";
	public static final String OUT_OF_BOUNDS = "OutOfBounds";
	public static final String OUT_OF_DOMAIN = "OutOfDomain";
	public static final String OVERGRAZED = "Overgrazed";
	public static final String PASSWORD_DOES_NOT_MATCH = "PasswordDoesNotMatch";
	public static final String READ_CLUB_IRREVOCABLY_REMOVED = "ReadClubIrrevocablyRemoved";
	public static final String READ_CLUB_REMOVED = "ReadClubRemoved";
	public static final String RECORDERS_STILL_OUTSTANDING = "RecordersStillOutstanding";
	public static final String REF_COUNT_BUG = "RefCountBug";
	public static final String REGION_RETURNED_NULL_STEPPER_EVEN_THOUGH_NON_EMPTY = "RegionReturnedNullStepperEvenThoughNonEmpty";
	public static final String REMOVE_FOULED = "RemoveFouled";
	public static final String SANITY_VIOLATION = "SanityViolation";
	public static final String SERVERLOOP = "Serverloop";
	public static final String SERVERLOOP_U_IS_U_NULL = "ServerloopIsNull";
	public static final String SIGNATURE_CLUB_IRREVOCABLY_REMOVED = "SignatureClubIrrevocablyRemoved";
	public static final String TOO_MANY_STEPPERS_RELEASED = "TooManySteppersReleased";
	public static final String TOO_MANY_ZEROS = "TooManyZeros";
	public static final String TURTLE_NOT_MATURE = "TurtleNotMature";
	public static final String UNENCODED_CATEGORY = "UnencodedCategory";
	public static final String UNKNOWN_COOKBOOK = "UnknownCookbook";
	public static final String UNINITIALIZED_PACKER = "UninitializedPacker";
	public static final String WORK_MUST_BE_GRABBED = "WorkMustBeGrabbed";
	public static final String WORK_IS_LOCKED_BY_SOMEONE_ELSE = "WorkIsLockedBySomeoneElse";
	public static final String WHO_SAYS = "WhoSays";
	public static final String WRONG_CHARACTER = "WrongCharacter";
	public static final String WRONG_COORD_SPACE = "WrongCoordSpace";
	public static final String ZERO_SEQUENCE = "ZeroSequence";

	/**
	 * Constructor for AboraRuntimeException.
	 */
	public AboraRuntimeException() {
		super();
	}

	/**
	 * Constructor for AboraRuntimeException.
	 * @param message
	 */
	public AboraRuntimeException(String message) {
		super(message);
	}

	/**
	 * Constructor for AboraRuntimeException.
	 * @param message
	 * @param cause
	 */
	public AboraRuntimeException(String message, Throwable cause) {
		super(message, cause);
	}

	/**
	 * Constructor for AboraRuntimeException.
	 * @param cause
	 */
	public AboraRuntimeException(Throwable cause) {
		super(cause);
	}

}
