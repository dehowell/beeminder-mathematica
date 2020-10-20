(* ::Package:: *)

BeginPackage["Beeminder`"];


BeeminderUser::usage = "BeeminderUser[] returns attributes of the authenticated user";


BeeminderGoal::usage = "BeeminderGoal[goal] returns attributes of the authenticated user's goal";


BeeminderGoals::usage = "BeeminderGoals[] returns all of the authenticated user's goals";


Begin["Private`"];


$BeeminderSystemCredentialKey = "BeeminderAuthToken";
$BeeminderBaseUrl="https://www.beeminder.com/api/v1";
$BeeminderApiTimeout = 2;


GetBeeminderCredentials[] := Module[
	{creds},
	creds = SystemCredential[$BeeminderSystemCredentialKey];

	If[MatchQ[creds,_Missing],
		AuthenticationDialog[
			{"username","auth_token"->""->"Masked"},
			SystemCredentialKey -> $BeeminderSystemCredentialKey,
			AppearanceRules -> {
				"Description" -> "Retrieve from https://www.beeminder.com/api/v1/auth_token.json"
			}
		];
		creds=SystemCredential[$BeeminderSystemCredentialKey],
		(* ELSE *)
		creds
	]
];

BeeminderAuthToken[] := GetBeeminderCredentials[]["Secret"];

BeeminderUsername[] := GetBeeminderCredentials[]["Data"]["username"];


MakeBeeminderAPIRequest[path:{_String..}] := MakeBeeminderAPIRequest[path,{}]

MakeBeeminderAPIRequest[
	path:{_String..},
	parameters:{Rule[_String,_]...}
] := Module[
	{url,request,response},
	url=URLBuild[
		{$BeeminderBaseUrl, Sequence@@path},
		{"auth_token" -> BeeminderAuthToken[] , Sequence@@parameters}
	];

	request = HTTPRequest[url, TimeConstraint -> $BeeminderApiTimeout];
	response = URLRead[request];

	If[response["StatusCode"] == 200,
		ImportByteArray[response["BodyByteArray"],"RawJSON"],
		Failure["HTTP Error", <|"Response"->response|>] (* TODO raise exception? *)
	]
];


BeeminderUser[] := MakeBeeminderAPIRequest[{"users", BeeminderUsername[]<>".json"}]


BeeminderGoal[goal_] := Module[
	{payload},
	payload = MakeBeeminderAPIRequest[{"users", BeeminderUsername[], "goals", goal<>".json"},{"datapoints"->True}];
	payload (* TODO convert to dataframes and other Mathematica native types *)
]


BeeminderGoals[] := Module[
	{payload},
	payload = MakeBeeminderAPIRequest[{"users", BeeminderUsername[], "goals.json"}];
	Association@Map[#["slug"] -> # &, payload]
]

End[];


EndPackage[];
