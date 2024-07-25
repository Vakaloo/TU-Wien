rml-mapper-example
--
An example application of [rml-mapper](https://github.com/RMLio/rmlmapper-java/).
* For a quick start, please take a look at the `src/main/java/Main.java`.
* Example input (`person.json`) and mappings (`person.rml.ttl`) from JSON files are provided.

You can execute the transformation using the following command on your terminal (the path to input file `person.json` 
is provided within `person.rml.ttl`): 
* ```java -jar lib/rmlmapper-6.1.3-r367-all.jar -m mapping.rml.ttl```
