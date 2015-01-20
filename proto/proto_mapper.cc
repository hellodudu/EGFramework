#include <google/protobuf/compiler/command_line_interface.h>
#include <google/protobuf/compiler/code_generator.h>
#include <google/protobuf/descriptor.h>
#include <string>
#include <iostream>

using namespace google::protobuf::compiler;
using namespace google::protobuf;
using namespace std;

class ErlangMapperGenerator : public CodeGenerator {
public:
  ErlangMapperGenerator();
  ~ErlangMapperGenerator();

  bool Generate(const FileDescriptor* file,
		const string& parameter,
		GeneratorContext* generator_context,
		string* error ) const;
private:
  GOOGLE_DISALLOW_EVIL_CONSTRUCTORS(ErlangMapperGenerator);
};

ErlangMapperGenerator::ErlangMapperGenerator(){}
ErlangMapperGenerator::~ErlangMapperGenerator(){}

bool ErlangMapperGenerator::Generate(const FileDescriptor* file,
				     const string& parameter,
				     GeneratorContext* generator_context,
				     string* error) const {
  int message_type_count = file->message_type_count();
  int id = atoi(parameter.c_str());
  ++id;
  for( int i = 0; i < message_type_count ; ++i ) {
    const Descriptor* descriptor = file->message_type(i);
    string messageName = descriptor->name();
    string output1 = string("get(")+to_string(id)+string(")->")+messageName+string("; ");
    string output2 = string("get(")+messageName+string(")->")+to_string(id)+string("; ");
    string output = output1+output2;
    ++id ;
    cout << output << endl;
  }
  return true ;
}

int main(int argc, char* argv[]){
  CommandLineInterface cli;
  ErlangMapperGenerator erlangMapperGenerator;
  
  cli.RegisterGenerator("--erlang_mapper_output", "--proto_id", &erlangMapperGenerator,"Erlang mapper.");  
  return cli.Run(argc,argv);
}

