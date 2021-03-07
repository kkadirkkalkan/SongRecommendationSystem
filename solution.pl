%  kadir kalkan
% 2019400258
% compiling: yes
% complete: yes

% artist(ArtistName, Genres, AlbumIds).
% album(AlbumId, AlbumName, ArtistNames, TrackIds).
% track(TrackId, TrackName, ArtistNames, AlbumName, [Explicit, Danceability, Energy,
%                                                    Key, Loudness, Mode, Speechiness,
%                                                    Acousticness, Instrumentalness, Liveness,
%                                                    Valence, Tempo, DurationMs, TimeSignature]).









features([explicit-0, danceability-1, energy-1,
         key-0, loudness-0, mode-1, speechiness-1,
      	  acousticness-1, instrumentalness-1,
          liveness-1, valence-1, tempo-0, duration_ms-0,
          time_signature-0]).

filter_features(Features, Filtered) :- features(X), filter_features_rec(Features, X, Filtered).  %filter the features
filter_features_rec([], [], []).
filter_features_rec([FeatHead|FeatTail], [Head|Tail], FilteredFeatures) :-
    filter_features_rec(FeatTail, Tail, FilteredTail),
    _-Use = Head,
  (
        (Use is 1, FilteredFeatures = [FeatHead|FilteredTail]);
       (Use is 0,
           FilteredFeatures = FilteredTail
        )
    ).
  
 my_length([],0).                      %finds length of a list
 my_length([_|L],N) :- my_length(L,N1), N is N1 + 1.
 
 divide_by_k([], _, []).               %divides by k member of a list
divide_by_k([Head|Tail], K, Result) :-
    Y is Head / K,
    divide_by_k(Tail, K, ResultTail),
    Result = [Y|ResultTail].

 sum(X,[],X).                          %summation of two lists elements with correspanding indexes
sum([],[],[]).
sum([H1| T1], [H2| T2], [ResH| ResT]) :- 
        sum(T1, T2, ResT),
        ResH is H1+H2.
 
 diff(X,[],X).                 % difference with two lists elements with correspanding indexes
diff([],[],[]).
diff([H1| T1], [H2| T2], [ResH| ResT]) :- 
        diff(T1, T2, ResT),
        ResH is H1-H2.
 
 squared([], []).            %square of list members
squared([L|Ls], [SqrdL|SqrdLs]) :-
    number(L),
    SqrdL is L * L,
    squared(Ls, SqrdLs).		

sum_member([], 0).             %sum of the list members
sum_member([H|T], Sum) :-
   sum_member(T, Rest),
   Sum is H + Rest.
  
%%%%%%%%%%%%%%%%%% getArtistTracks(+ArtistName, -TrackIds, -TrackNames) 5 points


getArtistTracks(ArtistName,TrackIds,TrackNames):-
    artist(ArtistName,_,AlbumIds),
    getAlbums(AlbumIds,TrackIds),
    getTracks(TrackIds,TrackNames).
getAlbums([],[]).                                %finds track ids of a list
getAlbums([AlbumHead|AlbumTail],TrackIds):-
    album(AlbumHead,_,_,TrackIds1),
    getAlbums(AlbumTail,TrackIds2),
    append(TrackIds1,TrackIds2,TrackIds).
getTracks([],[]).                               %finds track names of track ids
getTracks([TrackHead|TrackTail],TrackNames):-
    track(TrackHead,TrackNames1,_,_,_),
    getTracks(TrackTail,TrackNames2),
    append([TrackNames1],TrackNames2,TrackNames).


%%%%%%%%%%%%%%%%%%%%%% albumFeatures(+AlbumId, -AlbumFeatures) 5 point



albumFeatures(AlbumId, AlbumFeatures) :-
album(AlbumId,_,_,TrackIds),
length(TrackIds,X),
findtrackfatures(TrackIds,AlbumFeature),
divide_by_k(AlbumFeature,X,AlbumFeatures),!.

findtrackfatures([],[]).                    %summation of all albumfeatures elements with correspanding indexes
findtrackfatures([H|T],AlbumFeature):-
track(H,_,_,_,AlbumFeatures1),
filter_features(AlbumFeatures1,AlbumFeatures3),
findtrackfatures(T,AlbumFeatures2),
sum(AlbumFeatures3,AlbumFeatures2,AlbumFeature).


%%%%%%%%%%%%%%%%%%%5 artistFeatures(+ArtistName, -ArtistFeatures) 5 points

artistFeatures(ArtistName, ArtistFeatures):-
artist(ArtistName,_,AlbumIds),
getAlbum(AlbumIds,TrackIds),
my_length(TrackIds,X),
findartistfeatures(TrackIds,ArtistFeature),
divide_by_k(ArtistFeature,X, ArtistFeatures),!.
    
getAlbum([],[]).                          %finds trackids of listed albumids         
getAlbum([AlbumHead|AlbumTail],TrackIds):-
    album(AlbumHead,_,_,TrackIds1),
    getAlbum(AlbumTail,TrackIds2),
    append(TrackIds1,TrackIds2,TrackIds).

findartistfeatures([],[]).                  %summation of all albumfeatures elements with correspanding indexes
findartistfeatures([H|T],ArtistFeature):-
track(H,_,_,_,ArtistFeature1),
filter_features(ArtistFeature1,ArtistFeature3),
findartistfeatures(T,ArtistFeature2),
sum(ArtistFeature3,ArtistFeature2,ArtistFeature).

%%%%%%%%%%%%%%%% trackDistance(+TrackId1, +TrackId2, -Score) 5 points


trackDistance(TrackId1, TrackId2, Score):-
track(TrackId1,_,_,_,TrackFeature1),
filter_features(TrackFeature1,TrackFeatures1),
track(TrackId2,_,_,_,TrackFeature2),
filter_features(TrackFeature2,TrackFeatures2),
diff(TrackFeatures1,TrackFeatures2,Difference),
squared(Difference,Square),
sum_member(Square,SummofMembers),
Score is sqrt(SummofMembers).

%%%%%%%%%%%% albumDistance(+AlbumId1, +AlbumId2, -Score) 5 points


albumDistance(AlbumId1, AlbumId2, Score):-
albumFeatures(AlbumId1, AlbumFeatures1),
albumFeatures(AlbumId2, AlbumFeatures2),
findscore(AlbumFeatures1,AlbumFeatures2,Score),!.       

findscore(AlbumFeatures1,AlbumFeatures2,Score):-        %finds score between two AlbumFeaturess
diff(AlbumFeatures1,AlbumFeatures2,Difference),
squared(Difference,Square),
sum_member(Square,SummofMembers),
Score is sqrt(SummofMembers).


%%%%%%%%%% artistDistance(+ArtistName1, +ArtistName2, -Score) 5 points

artistDistance(ArtistName1,ArtistName2, Score):-
artistFeatures(ArtistName1, ArtistFeatures1),
artistFeatures(ArtistName2, ArtistFeatures2),
findscore(ArtistFeatures1,ArtistFeatures2,Score),!.


%%%%%%%%%%% findMostSimilarTracks(+TrackId, -SimilarIds, -SimilarNames) 10 points


   take(Src,N,L) :- findall(E, (nth1(I,Src,E), I =< N), L).                %takes first N elements of a list.
   
    findMostSimilarTracks(TrackId,SimilarIds, SimilarNames):-
   findall( Distance, ( trackDistance(TrackId,SimilarIds1,Distance), \+(SimilarIds1=TrackId) ),Distance),
    sort(Distance,Distance1),
	take(Distance1,30,Distance2),
	findSimilarIds(Distance2,TrackId,SimilarIds),
	getTrack(SimilarIds,SimilarNames).
	
	                                     
	findSimilarIds([],_,[]).                       %find SimilarIds with correspanding distance between SimilarIds and TrackId
	findSimilarIds([H|T],TrackId,SimilarIds):-
	trackDistance(TrackId,SimilarIds1,H),
	findSimilarIds(T,TrackId,SimilarIds2),
	append([SimilarIds1],SimilarIds2,SimilarIds).
	
	getTrack([],[]).                            %finds similarnames of similarids.
    getTrack([TrackHead|TrackTail],TrackNames):-
    track(TrackHead,TrackNames1,_,_,_),
    getTrack(TrackTail,TrackNames2),
    append([TrackNames1],TrackNames2,TrackNames).
	
	

		
	
%%%%%%%%%%%%%%%%%% findMostSimilarAlbums(+AlbumId, -SimilarIds, -SimilarNames) 10 points

   
   
   
    findMostSimilarAlbums(AlbumId, SimilarIds, SimilarNames):-
    findall(X-Y,(album(Y,_,_,_),albumDistance(AlbumId,Y,X), \+(Y=AlbumId)),Distance),
	msort(Distance,Distance1),
	take(Distance1,30,Distance2),
	findIds(Distance2,SimilarIds),
	getAlbum1(SimilarIds,SimilarNames).
    
	findIds([],[]).                          %finds Y from X-Y distance list
	findIds([_-A|T],SimilarIds):-
	findIds(T,Ids1),
	SimilarIds=[A|Ids1].
	
	
	getAlbum1([],[]).                                    %finds SimilarNamess of SimilarIds.
    getAlbum1([AlbumHead|AlbumTail],SimilarNames):-
    album(AlbumHead,AlbumNames1,_,_),
    getAlbum1(AlbumTail,AlbumNames2),
    append([AlbumNames1],AlbumNames2,SimilarNames).
	
	

%%%%%%%%%%%%%%%%%%%%%%%% findMostSimilarArtists(+ArtistName, -SimilarArtists) 10 points


    findMostSimilarArtists(ArtistName, SimilarArtists):-
    findall(X-Y,(artist(Y,_,_),artistDistance(ArtistName,Y,X), \+(Y=ArtistName)),Distance),
	msort(Distance,Distance1),
	take(Distance1,30,Distance2),
	findIds1(Distance2,SimilarArtists).
	
	findIds1([],[]).                              %finds Y from X-Y distance list
	findIds1([_-A|T],SimilarArtists):-
	findIds1(T,Ids1),
	SimilarArtists=[A|Ids1].
	
	


%%%%%%%%%%%%%%%%%%%%%%%%%%%% filterExplicitTracks(+TrackList, -FilteredTracks) 5 points


same([F1|_]):-    %turns true if first member of a list is true
    F1 = 0.
    


filterExplicitTracks(TrackList,FilteredTracks ):-
 findfeatures(TrackList,TrackFeatures ),
findfiltered(TrackFeatures,FilteredTracks).

findfeatures([],[]).                         %finds explicit TrackFeatures from TrackList
findfeatures([H|T],TrackFeatures):-
findall(Y,  ( track(H,_,_,_,Y), same(Y) ), TrackFeatures1  ),
findfeatures(T,TrackFeatures2),
append(TrackFeatures1,TrackFeatures2,TrackFeatures).

findfiltered([],[]).                     %finds trackid of explicit TrackFeatures
findfiltered([H|T],FilteredTracks):-
track(FilteredTracks1,_,_,_,H),
findfiltered(T,FilteredTracks2),
append([FilteredTracks1],FilteredTracks2,FilteredTracks).




%%%%%%%%%%%%%%%%%% getTrackGenre(+TrackId, -Genres) 5 points


getTrackGenre(TrackId, Genres):-
 
 track(TrackId,_,Artists,_,_),
findGenres(Artists, Genres).


findGenres([],[]).                   %finds artist's genres
findGenres([H|T], Genres):-
artist(H,Genres1,_),
findGenres(T,Genres2),
append(Genres1,Genres2,Genres).








%%%%%%%%%%%%%%%%%%% discoverPlaylist(+LikedGenres, +DislikedGenres, +Features, +FileName, -Playlist) 30 points


 discoverPlaylist(LikedGenres,DislikedGenres,Feature, FileName, Playlist):-
 subString3(Ids1,LikedGenres), 
 subString3(Ids2,DislikedGenres), 
 subtract(Ids1,Ids2,Ids),             
 
 findDistances(Feature,Ids,Distances),
 sort(Distances,Distance1),
 take(Distance1,30,Distance2),
 findSimilarIds1(Distance2,Feature,Playlist),
 findnamess(ArtistNames,Playlist),
 findnamesss(TrackNames, Playlist),
 open(FileName, write, Stream),
 writeln(Stream, Playlist), 
 writeln(Stream, ArtistNames),
 writeln(Stream, TrackNames),
 writeln(Stream, Distance2),
 close(Stream).
 
	
 
 
 findDistances(_,[],[]).                    %finds tracakdistance between member of Ids and given Feaature
 findDistances(Feature,[H|T],Distances):-
 trackDistance1(Feature,H,Distances1),
 findDistances(Feature,T,Distances2),
 append([Distances1],Distances2,Distances).
 
 
 
 
 findSimilarIds1([],_,[]).                   %finds trackids whic have distance between Features.
	findSimilarIds1([H|T],Feature,Playlist):-
	trackDistance1(Feature,SimilarIds1,H),
	findSimilarIds1(T,Feature,SimilarIds2),
	append([SimilarIds1],SimilarIds2,Playlist).
 
 
 
 subString3([],[]).               %find track ids whoses subtring is LikesGenres or DislikedGenres
subString3(Ids,[H|T]):-
findall(X,( track(X,_,_,_,_) ,   getTrackGenre(X,Genres),   isSubString(Genres,H)    ), Ids1),
subString3(Ids2,T),
union(Ids1,Ids2,Ids).

 isSubString([],[]).                 %control of is the string is substring of Genres.
 isSubString(Genres,H):-
 atomics_to_string(Genres,String),
sub_string(String,_,_,_,H).


 
trackDistance1(Feature, TrackId2, Score):-                %finds trackdifference between Feature and similarids's trackfeature
track(TrackId2,_,_,_,TrackFeature2),
filter_features(TrackFeature2,TrackFeatures2),
diff(Feature,TrackFeatures2,Difference),
squared(Difference,Square),
sum_member(Square,SummofMembers),
Score is sqrt(SummofMembers).



findnamesss([], []).
findnamesss(TrackNames, [H|T]):-
track(H,TrackNames1,_,_,_),
findnamesss(TrackNames2, T),
append([TrackNames1],TrackNames2,TrackNames).


findnamess([], []).
findnamess(ArtistNames,[H|T]):-
track(H,_,ArtistNames1,_,_),
findnamess(ArtistNames2,T),
append([ArtistNames1],ArtistNames2,ArtistNames).




%%%%%%%%%%%%%%%%%%%%%%%%

