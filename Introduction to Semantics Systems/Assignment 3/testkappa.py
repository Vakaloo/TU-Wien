from rdflib import Graph, URIRef, BNode, Literal, Namespace
from rdflib.namespace import FOAF, RDF, RDFS, OWL

import owlrl 
import pprint
import os

film = Namespace("http://semantics.id/ns/example/film/ontology.ttl#")
#film = "C:/Users/vaka1/Desktop/film.ttl"
#film_instances = "C:/Users/vaka1/Desktop/film-instances.ttl"
g = Graph()
#g.namespace_manager.bind('EX', film)
g.parse(film)
print(len(g))
#owlrl.DeductiveClosure(owlrl.RDFS_Semantics).expand(g)

for (sub,ored,obj) in g.triples((None, RDF.type , OWL.Class)):
    print(sub)


#Export Graph
#g.serialize(format="n3", destination="C:/Users/vaka1/Desktop/export.n3")
#print(g.serialize(format="turtle"))
#print(g.serialize(format="json-ld"))

#g.add((RDF.Property, RDF.type, RDFS.Class))

##### Add class #####
'''
#print(len(g))
#print(len(g))
class_node = URIRef("Malaka" + "ClassName")
g.add((class_node, RDF.type, OWL.Class)) 
g.add((class_node, RDFS.label, Literal("ClassName"))) 
#print(g.serialize(format="turtle"))
print(len(g))
'''
##### Add Property #####
'''
#print(len(g))
addProperty = URIRef(film + "PropertyName")
domain = URIRef(film + "Film")
range = URIRef(film + "Actor")
g.add((addProperty, RDF.type, OWL.ObjectProperty)) 
g.add((addProperty, RDFS.label, Literal("PropertyName"))) 
g.add((addProperty, RDFS.domain, domain)) 
g.add((addProperty, RDFS.range, range)) 
print(g.serialize(format="turtle"))
#print(len(g))
'''

##### Add Instance #####
'''
print(len(g))
addInstance = URIRef(film + "Instance")
class_instantiate = URIRef(film + "Actor")
g.add((addInstance, RDF.type, OWL.NamedIndividual)) 
g.add((addInstance, RDF.type, class_instantiate)) 
print(g.serialize(format="turtle"))
print(len(g))

g.serialize(format="ttl", destination="C:/Users/Vaka1/Desktop/export.ttl")
'''

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


'''
print(len(queries))
for i in range(len(queries)):
    result = g.query(queries[i])
    print("--- "+queries[i]+" ---")
    for row in result:
        print(row)
'''

'''
for i in range(len(queries)):
    print ("Query: %s " % (i+1))
    print("====================")
    print(queries[i])
'''

'''
print(len(queries))
print("Please chooce a number from 1 to 6")
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
'''

'''
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
    final_destination = dest + "/export.ttl"

    g.query(queries[choice_query-1]).serialize(format="turtle", 
            destination=final_destination)

'''


'''
describe ?film
where {
	?film rdfs:label "Dune";
     		:releaseYear 1984.
}

,

describe ?actor
where {
	?film :hasActor ?actor;
    	rdfs:label "Dune";
     		:releaseYear 1984
}
'''