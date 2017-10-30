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

% FIXME the query parameters used for the call should be figured out from triples in the service endpoint
tpfuri_SPO(S, P, O, URI) :- atom(S), atom(P), atom(O), uri_query_components(QueryString, [subject=S, predicate=P, object=O]), tpf_endpoint(Endpoint), atomic_concat(Endpoint, QueryString, URI).
tpfuri_SP(S, P, URI) :- atom(S), atom(P), uri_query_components(QueryString, [subject=S, predicate=P]), tpf_endpoint(Endpoint), atomic_concat(Endpoint, QueryString, URI).
tpfuri_SO(S, O, URI) :- atom(S), atom(O), uri_query_components(QueryString, [subject=S, object=O]), tpf_endpoint(Endpoint), atomic_concat(Endpoint, QueryString, URI).
tpfuri_PO(P, O, URI) :- atom(P), atom(O), uri_query_components(QueryString, [predicate=P, object=O]), tpf_endpoint(Endpoint), atomic_concat(Endpoint, QueryString, URI).
tpfuri_S(S, URI) :- atom(S), uri_query_components(QueryString, [subject=S]), tpf_endpoint(Endpoint), atomic_concat(Endpoint, QueryString, URI).
tpfuri_P(P, URI) :- atom(P), uri_query_components(QueryString, [predicate=P]), tpf_endpoint(Endpoint), atomic_concat(Endpoint, QueryString, URI).
tpfuri_O(O, URI) :- atom(O), uri_query_components(QueryString, [object=O]), tpf_endpoint(Endpoint), atomic_concat(Endpoint, QueryString, URI).
tpfuri(URI) :- tpf_endpoint(URI).

:- discontiguous rdf/3.
rdf(S, P, O) :- atom(S), atom(P), atom(O), tpfuri_SPO(S, P, O, URI), has_triples(URI, T), member(rdf(S, P, O), T).
rdf(S, P, O) :- atom(S), atom(P), var(O), tpfuri_SP(S, P, URI), has_triples(URI, T), member(rdf(S, P, O), T).
rdf(S, P, O) :- atom(S), var(P), atom(O), tpfuri_SO(S, O, URI), has_triples(URI, T), member(rdf(S, P, O), T).
rdf(S, P, O) :- var(S), atom(P), atom(O), tpfuri_PO(P, O, URI), has_triples(URI, T), member(rdf(S, P, O), T).
rdf(S, P, O) :- atom(S), var(P), var(O), tpfuri_S(S, URI), has_triples(URI, T), member(rdf(S, P, O), T).
rdf(S, P, O) :- var(S), atom(P), var(O), tpfuri_P(P, URI), has_triples(URI, T), member(rdf(S, P, O), T).
rdf(S, P, O) :- var(S), var(P), atom(O), tpfuri_O(O, URI), has_triples(URI, T), member(rdf(S, P, O), T).
rdf(S, P, O) :- var(S), var(P), var(O), tpfuri(URI), has_triples(URI, T), member(rdf(S, P, O), T).


% Examples

movie_starring_Bill_Murray(MovieID, Title, Director) :- 
  rdf(MovieID, 'http://dbpedia.org/ontology/starring', 'http://dbpedia.org/resource/Bill_Murray'),
  rdf(MovieID, 'http://www.w3.org/2000/01/rdf-schema#label', Title),
  rdf(MovieID, 'http://dbpedia.org/ontology/director', DirectorID),
  rdf(DirectorID, 'http://www.w3.org/2000/01/rdf-schema#label', Director).
