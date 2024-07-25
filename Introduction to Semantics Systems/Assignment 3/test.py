from rdflib import Graph, URIRef, BNode, Literal, Namespace
from rdflib.namespace import FOAF, RDF, RDFS, OWL

import owlrl 
import pprint
import os
import time

ontology = "http://semantics.id/ns/example/film/ontology.ttl#" #global variable
queries = ['''
    select ?film ?title
    where { 
        ?film rdf:type :Film;
        rdfs:label ?title
    }
    ''',
    '''
    select ?studio
    where {
        ?studio a :FilmStudio.
        ?studio :establishedDate ?date.
        FILTER (?date > "1960-01-01"^^xsd:date)   
    }
    ''',
    '''
    select ?title ?genre_name
    where {
        ?film rdf:type :Film;
            rdfs:label ?title.
        ?film :hasGenre ?genre.
        ?genre rdfs:label ?genre_name.
        filter(?genre_name = "Action" || ?genre_name = "Family")   
    }
    ''',
    '''
    select ?name_actor
    where {
            ?actor a :Actor;
            :fullName ?name_actor;
            :dateOfBirth ?birthdate
    } order BY ASC(?birthdate)
    ''',
    '''
    select ?movie_name (count(?actor) as ?number_of_actors)
    where {
        ?movie a :Film;
        rdfs:label ?movie_name.
        ?movie :hasActor ?actor
        filter(?movie_name != "Dune")
    } Group by ?movie_name
    ''',
    '''
    select ?person ?movie_title
    where {
        ?movie a :Film
        {?movie :hasActor ?person.
        ?person :fullName ?name_person.
        ?movie rdfs:label ?movie_title}
        union
        {?movie :hasCrew ?person.
        ?person :fullName ?name_person.
        ?movie rdfs:label ?movie_title}
    }order by ?name_person
    ''',
    '''ask { 
        ?film rdf:type :Film;
            rdfs:label "Dune"
    }
    ''',
    '''
    Construct{
        ?writer :works ?studio
    }
    where {
        ?film :hasScriptWriter ?writer.
        ?film :hasFilmStudio ?studio.  
    }
    ''',
    '''
    Construct{
        ?actor :preferGenre ?genre
    }
    where {
        ?film :hasGenre ?genre.
        ?film :hasActor ?actor. 
    }
    '''
    ]

def LoadOntology():
    ontology = input("Load your ontology from the URI or from a file with the destination included: ")
    g = Graph()
    g.parse(ontology)
    return g

def ActivateReasoner(g):
    owlrl.DeductiveClosure(owlrl.RDFS_Semantics).expand(g)

def ExportGraph(g):
    format = ["turtle", "pretty-xml", "json-ld", "nt", "n3", "trig", "trix", "nquads"]
    format_graph = [".ttl", ".xml", ".jsonld", ".nt", ".n3", ".trig", ".trix", ".nquads"]
    print ("1. Turtle")
    print ("2. RDF/XML")
    print ("3. JSON-LD")
    print ("4. N-Triples")
    print ("5. Notation-3")
    print ("6. Trig")
    print ("7. Trix")
    print ("8. N-Quads")
    graph_choice = int(input("Choose the format: "))
    if graph_choice in range(1,8):
        dest = str(input("Choose the destination that you want to export the RDF graph: "))
        final_destination = dest + "/export" + format_graph[graph_choice-1]
        g.serialize(format=format[graph_choice-1], destination=final_destination)
    else:
        print("Invalid choice!")


def AddClass():
    g = Graph()
    g.parse(ontology)

    name = str(input("Give the name of the class that you want to create: "))
    class_node = URIRef(ontology + name)

    g.add((class_node, RDF.type, OWL.Class)) 
    g.add((class_node, RDFS.label, Literal(name)))

def AddPRoperty():
    g = Graph()
    g.parse(ontology)

    propertyname = str(input("Give the name of the property that you want to create: "))
    domainname = str(input("Give the domain: "))
    rangename = str(input("Give the range: "))

    addProperty = URIRef(ontology + propertyname)
    domain = URIRef(ontology + domainname)
    range = URIRef(ontology + rangename)

    g.add((addProperty, RDF.type, OWL.ObjectProperty)) 
    g.add((addProperty, RDFS.label, Literal(propertyname))) 
    g.add((addProperty, RDFS.domain, domain)) 
    g.add((addProperty, RDFS.range, range))


def AddInstance():
    g = Graph()
    g.parse(ontology)

    nameinstance = str(input("Give the name of the instance that you want to create: "))
    nameclass = str(input("Give the name of the class that the chosen instance belongs to: "))

    instance = URIRef(ontology + nameinstance)
    class_instantiate = URIRef(ontology + nameclass)

    g.add((instance, RDF.type, OWL.NamedIndividual)) 
    g.add((instance, RDF.type, class_instantiate)) 
    print(g.serialize(format="turtle"))


def ShowQueries():
    for i in range(len(queries)):
        print ("Query: %s " % (i+1))
        print("====================")
        print(queries[i])

def ExecuteSelectQuery():
    g = Graph()
    g.parse(ontology)
    print("For the select queries, please chooce a number from 1 to 6")
    choice_query = int(input("Choose your select query: "))
    if choice_query!=7 and choice_query!=8 and choice_query!=9:
        print("The query that you choose is:")
        print(queries[choice_query-1])
        print("The results are:")
        result = g.query(queries[choice_query-1])
        for row in result:
            print(row)
    else:
        print("You did not choose a Select Query!")

def ExecuteConstructQuery():
    g = Graph()
    g.parse(ontology)
    print("Please chooce 8 or 9 to execute a Costruct Query")
    choice_query = int(input("Choose your select query: "))
    if choice_query==8 or choice_query==9:
        print("The query that you choose is:")
        print(queries[choice_query-1])
        print("The results are:")
        result = g.query(queries[choice_query-1])
    for row in result:
        print(row)

    dest = str(input("Choose the destination that you want to save the results: "))
    final_destination = dest + "/Construct_result.ttl"

    g.query(queries[choice_query-1]).serialize(format="turtle", 
            destination=final_destination)

menu_chosen=True
while menu_chosen:
    print ("MAIN MENU")
    time.sleep(1)
    print ("-----------------")
    print ("1. Load Ontology")
    print ("2. Activate Reasoner")
    print ("3. Export Graph")
    print ("4. Add Class")
    print ("5. Add Property")
    print ("6. Add Instance")
    print ("7. Show Queries")
    print ("8. Execute Select Query")
    print ("9. Execute Construct Query")
    print ("10. Exit")
    print ("-----------------")

    menu_chosen = int(input("Choose your menu option: "))

    if menu_chosen == 1:
        try:
            graph = LoadOntology()
            print("Your ontology has been loaded.\n")
        except:
            print("There has been an error loading your ontology. Please try again!")
    elif menu_chosen == 2:
        try:
            graph = ActivateReasoner(graph)
            print("The Reasoner has been activated.\n")
        except:
            print("There has been an error activating the reasoner. Please try again!")
    elif menu_chosen == 3:
        try:
            ExportGraph(graph)
            print("The Graph has been exported.\n")
        except:
            print("There has been an error exporting the graph. Please try again!")
    elif menu_chosen == 4:
        try:
            AddClass()
            print("A Class has been added.\n")
        except:
            print("There has been an error adding a new class. Please try again!")
    elif menu_chosen == 5:
        try:
            AddPRoperty()
            print("A Property has been added.\n")
        except:
            print("There has been an error adding a new property. Please try again!")
    elif menu_chosen == 6:
        try:
            AddInstance()
            print("An Instance has been added.\n")
        except:
            print("There has been an error adding a new instance. Please try again!")
    elif menu_chosen == 7:
        ShowQueries()
    elif menu_chosen == 8:
        try:
            ExecuteSelectQuery()
        except:
            print("There has been an error executing the select query. Please try again!")
    elif menu_chosen == 9:
        try:
            ExecuteConstructQuery()
        except:
            print("There has been an error executing the execute query. Please try again!")
    elif menu_chosen == 10:
        print("\n Goodbye")
        menu_chosen = False
    else:
        print("\n Not Valid Choice Try again \n")


