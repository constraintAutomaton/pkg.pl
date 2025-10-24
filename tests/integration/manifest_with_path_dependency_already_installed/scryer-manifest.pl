name("manifest_with_no_dependencies").
main_file("main.pl").
license(name("UNLICENSE")).
dependencies([
    dependency("test", path("./local_package"))
]).
