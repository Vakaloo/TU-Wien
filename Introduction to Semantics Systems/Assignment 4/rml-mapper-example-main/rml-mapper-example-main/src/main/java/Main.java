import be.ugent.rml.Executor;
import be.ugent.rml.records.RecordsFactory;
import be.ugent.rml.store.QuadStore;
import be.ugent.rml.store.QuadStoreFactory;
import be.ugent.rml.store.RDF4JStore;
import be.ugent.rml.term.NamedNode;

import java.io.*;

// adapted from https://github.com/RMLio/rmlmapper-java/blob/master/src/test/java/be/ugent/rml/readme/ReadmeTest.java
public class Main {

    public static void main(String[] args) throws Exception {

        // Get the mapping string stream
        InputStream mappingStream = new FileInputStream("person.rml.ttl");

        // Load the mapping in a QuadStore
        QuadStore rmlStore = QuadStoreFactory.read(mappingStream);

        // Set up the basepath for the records factory, i.e., the basepath for the (local file) data sources
        RecordsFactory factory = new RecordsFactory(".");

        // Set up the outputstore (needed when you want to output something else than nquads
        QuadStore outputStore = new RDF4JStore();

        // Create the Executor
        Executor executor = new Executor(rmlStore, factory, outputStore, null, null);

        // Execute the mapping
        QuadStore result = executor.execute(null).get(new NamedNode("rmlmapper://default.store"));

        // Output the result
        FileWriter out = new FileWriter("output/person-output.ttl");
        result.write(out, "turtle");
        out.close();
    }
}
