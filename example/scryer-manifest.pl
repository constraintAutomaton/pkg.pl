name("name_of_the_package").
% Optional. The file that will be imported when this package is used.
main_file("main.pl").
% Optional
dependencies([
    % A git url to clone
    git("https://github.com/bakaq/testing.pl.git"),
    % A git url to clone at a specific branch
    git("https://github.com/constraintAutomaton/test-prolog-package-manager.git", branch("branch")),
    % A git url to clone at a tag
    git("https://github.com/constraintAutomaton/test-prolog-package-manager.git", tag("tag")),
    % A git url to clone at a specific commit hash
    git("https://github.com/constraintAutomaton/test-prolog-package-manager.git", hash("d19fefc1d7907f6675e181601bb9b8b94561b441"))
]).