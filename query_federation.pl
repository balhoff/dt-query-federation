:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(tabling)).
:- use_module(library(semweb/turtle)).
:- use_module(library(uri)).


tpf_endpoint('http://fragments.dbpedia.org/2016-04/en?').
tpf_endpoint('http://data.linkeddatafragments.org/lov?').

:- table downloaded_triples/2.
downloaded_triples(URI, T) :- setup_call_cleanup(http_open(URI, In, [request_header('Accept'='text/turtle')]), rdf_read_turtle(In, T, []), close(In)).
has_download_uri(Triples, Other) :- member(rdf(_, 'http://www.w3.org/ns/hydra/core#next', Other), Triples).
has_triples(URI, Triples) :- downloaded_triples(URI, Triples).
has_triples(URI, MoreTriples) :- downloaded_triples(URI, Triples), has_download_uri(Triples, Other), has_triples(Other, MoreTriples).

rdf(S, P, O) :-
        findall(A=V,(member(A=V,[subject=S,predicate=P,object=O]),atom(V)),AVs),
        uri_query_components(QueryString, AVs),
        tpf_endpoint(Endpoint),
        atomic_concat(Endpoint, QueryString, URI),
        has_triples(URI, T), member(rdf(S, P, O), T).

% Examples

movie_starring_Bill_Murray(MovieID, Title, Director) :- 
  rdf(MovieID, 'http://dbpedia.org/ontology/starring', 'http://dbpedia.org/resource/Bill_Murray'),
  rdf(MovieID, 'http://www.w3.org/2000/01/rdf-schema#label', Title),
  rdf(MovieID, 'http://dbpedia.org/ontology/director', DirectorID),
  rdf(DirectorID, 'http://www.w3.org/2000/01/rdf-schema#label', Director).
