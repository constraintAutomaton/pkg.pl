name("name_of_the_package").
main_file("main.pl").
license(name("UNLICENSE"), path("./do_not_exist")).
dependencies([
    dependency("test", git("https://github.com/constraintAutomaton/test-prolog-package-manager.git"))
]).
