%%
%% SSDP
%%

-define(UPNP, "UPnP/1.0 CELL/0.1").

-define(MULTICAST_GROUP, {239,255,255,250}).
-define(MULTICAST_PORT, 1900).

-define(SOCKET_OPTIONS, [
	  {broadcast, true},
	  {reuseaddr, true},
	  {multicast_loop, false},   % was true
	  {multicast_if, ssdp_os_info:get_ip()},
	  {multicast_ttl, 32} ]).

-define(ERLMEDIASERVER_APP_FILE, erlmediaserver).

%% root device
-record(rootdevice, {
   uuid, wirelessmode, port, uri,
   descriptionuri="/services", services=[], os, ip,
   rootdevice="upnp:rootdevice", elementname="device" }).

-record(result, {container, item}).
-record(searchArguments, {containerID, searchCriteria, filter, startingIndex, requestedCount, sortCriteria}).
-record(searchResult, {result, numberReturned, totalMatches, updateID}).
-record(browseArguments, {objectID, browserFlag, filter, startingIndex, requestedCount, sortCriteria}).
-record(browseResult, {result, numberReturned, totalMatches}).
%%
%% Funktionen
%%
%%-define(TREEWALKERFUN, fun(X) -> filelib:is_dir(filename:join(StartDir, X)) and (string:sub_string(filename:basename(X),1,1) /= ".") end).
-define(TREEWALKERFUN, fun(X) -> io:format("F ~p~n ", [X]),filelib:is_dir(X) and (string:sub_string(filename:basename(X),1,1) /= ".") end).
-define(FILELIST(X), filelib:fold_files(X, ".xml$", false, fun(F, L) -> [F|L] end, [])).
%%
%% XML
%%
-define(DIDL_LITE_HEADER, "<DIDL-Lite xmlns=\"urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:upnp=\"urn:schemas-upnp-org:metadata-1-0/upnp/\"/>\n").
-define(DIDL_LITE_TRAILER, "</DIDL-Lite>").
-define(SOAPENVELOPE,"<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\" s:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\">\n").
%%
%% Konstanten
%%
-define(ALLSTATIONS, "Allstations").
-define(GENRES, "Genres").
-define(NO_SUCH_OBJECT,7 01).
-define(INVALID_CURRENT_TAG, 702).
-define(INVALID_NEW_TAG, 703).
-define(REQUIRED_TAG, 704).
-define(READ_ONLY_TAG, 705).
-define(PARAMETER_MISMATCH, 706).
-define(INVALID_SEARCH_CRITERIA, 708).
-define(INVALID_SORT_CRITERIA, 709).
-define(NO_SUCH_CONTAINER, 710).
-define(RESTRICTED_OBJECT, 711).
-define(BAD_METADATA, 712).
-define(RESTRICTED_PARENT_OBJECT, 713).
-define(NO_SUCH_SOURCE_RESOURCE, 714).
-define(SOURCE_RESOURCE_ACCESS_DENIED, 715).
-define(TRANSFER_BUSY, 716).
-define(NO_SUCH_FILE_TRANSFER, 717).
-define(NO_SUCH_DEST_RESOURCE, 718).
-define(DEST_RESOURCE_ACCESS_DENIED, 719).
-define(CANNOT_PROCESS_REQUEST, 720).
