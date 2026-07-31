{
  description = "cl-cc AST node types and protocol (ast-children, ast-bound-names)";

  inputs = {
    # nixos-unstable, not nixpkgs-unstable: it advances only after the NixOS
    # release tests pass, so it is less likely to land a broken build.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Pinned to a release tag. A bare `github:nerima-lisp/cl-weave` follows
    # that repository's default branch, so an upstream push to main would
    # break this repository's CI without warning. This used to be a
    # `flake = false` source tree handed to the runner through an environment
    # variable; it is a normal flake input now, which is what lets ASDF find
    # cl-weave through CL_SOURCE_REGISTRY like any other dependency.
    # cl-cc-bootstrap has cut no tagged release yet, so this necessarily
    # follows its default branch; every other input below has a tag and is
    # pinned to one, for the reason cl-weave's comment gives.
    cl-cc-bootstrap = {
      url = "github:nerima-lisp/cl-cc-bootstrap";
      flake = false;
    };
    # cl-cc-runtime has a v0.1.0 tag, but it predates the stack-segment
    # continuation-snapshot and mmap-backed-stack work this repository's own
    # test suite already exercises (cl-cc-runtime commits 9094079/e1bc75f,
    # both after the v0.1.0 tag). Pinning to v0.1.0 would be a regression, so
    # this follows the default branch until a release includes that work.
    cl-cc-runtime = {
      url = "github:nerima-lisp/cl-cc-runtime";
      flake = false;
    };
    # cl-cc-runtime's own dependencies; ASDF resolves them off the same registry.
    cl-log-kit = {
      url = "github:nerima-lisp/cl-log-kit/v2.0.0";
      flake = false;
    };
    cl-process-kit = {
      url = "github:nerima-lisp/cl-process-kit/v2.0.0";
      flake = false;
    };
    cl-json-kit = {
      url = "github:nerima-lisp/cl-json-kit/v1.0.1";
      flake = false;
    };
    cl-boundary-kit = {
      url = "github:nerima-lisp/cl-boundary-kit/v1.0.0";
      flake = false;
    };
    # cl-log-kit 2.0.0's runtime deps: it stopped being zero-dependency in
    # favor of these three nerima-lisp packages, used directly with no
    # adapter layer (see cl-log-kit's CHANGELOG.md, [2.0.0]/BREAKING).
    cl-date-kit = {
      url = "github:nerima-lisp/cl-date-kit/v0.2.0";
      flake = false;
    };
    cl-concurrent-kit = {
      url = "github:nerima-lisp/cl-concurrent-kit/v0.1.0";
      flake = false;
    };
    cl-host-kit = {
      url = "github:nerima-lisp/cl-host-kit/v0.2.1";
      flake = false;
    };
    # This repository's own guest-visible regex stdlib (src/regex.lisp) is
    # cl-regex-kit directly -- no hand-rolled matcher, no adapter layer.
    # cl-regex-kit has cut no tagged release yet, so it follows its default
    # branch like cl-cc-bootstrap above; cl-parser-kit is its own tokenizer
    # dependency and does have a tag.
    cl-regex-kit = {
      url = "github:nerima-lisp/cl-regex-kit";
      flake = false;
    };
    cl-parser-kit = {
      url = "github:nerima-lisp/cl-parser-kit/v1.0.1";
      flake = false;
    };
    # vm-terminal.lisp's raw-mode entry/exit and terminal-size queries are
    # cl-tty-kit's WITH-RAW-MODE/TERMINAL-SIZE directly, replacing what used
    # to shell out to stty(1). Zero dependencies of its own.
    cl-tty-kit = {
      url = "github:nerima-lisp/cl-tty-kit/v1.0.3";
      flake = false;
    };
    cl-weave = {
      url = "github:nerima-lisp/cl-weave/v1.1.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      cl-weave,
      cl-cc-bootstrap,
      cl-cc-runtime,
      cl-log-kit,
      cl-process-kit,
      cl-json-kit,
      cl-boundary-kit,
      cl-date-kit,
      cl-concurrent-kit,
      cl-host-kit,
      cl-regex-kit,
      cl-parser-kit,
      cl-tty-kit,
      treefmt-nix,
      ...
    }:
    let
      # Only platforms that something actually verifies are declared.
      # x86_64-linux is what CI runs; aarch64-darwin is the development
      # machine, so it is verified on every local `nix flake check`. Dropping
      # aarch64-darwin would make that local run skip every derivation and
      # still report success. aarch64-linux and x86_64-darwin are declared by
      # nobody's verification, so they are not declared here either.
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;

      # CL_SOURCE_REGISTRY for the test, coverage and dev environments.
      sourceRegistry = "${cl-weave}//:${cl-cc-bootstrap}//:${cl-cc-runtime}//:${cl-log-kit}//:${cl-process-kit}//:${cl-json-kit}//:${cl-boundary-kit}//:${cl-date-kit}//:${cl-concurrent-kit}//:${cl-host-kit}//:${cl-regex-kit}//:${cl-parser-kit}//:${cl-tty-kit}//:${self}//";

      # Single source of truth for the package version: the `:version` form in
      # cl-cc-vm.asd. A release only ever edits the .asd file and every Nix
      # package (default + docs) follows automatically. Nix regexes are
      # whole-string anchored and `.` never spans newlines, so the version is
      # extracted line-by-line rather than with one multi-line match.
      version =
        let
          lines = nixpkgs.lib.splitString "\n" (builtins.readFile ./cl-cc-vm.asd);
          versionLine = builtins.head (
            builtins.filter (line: builtins.match "[[:space:]]*:version \"[^\"]*\"" line != null) lines
          );
        in
        builtins.head (builtins.match "[[:space:]]*:version \"([^\"]*)\"" versionLine);

      # treefmt drives `nix fmt` and the `checks.<system>.formatting` gate.
      # Scope is Nix only: nixfmt (RFC-style) is a zero-footgun, low-diff
      # formatter, whereas YAML formatters mangle the GitHub Actions `on:`
      # key and Markdown reformatting would churn the whole docs tree.
      treefmtEval = forAllSystems (
        system:
        treefmt-nix.lib.evalModule nixpkgs.legacyPackages.${system} {
          projectRootFile = "flake.nix";
          programs.nixfmt.enable = true;
        }
      );
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        rec {
          cl-cc-vm = pkgs.sbcl.buildASDFSystem {
            pname = "cl-cc-vm";
            inherit version;
            src = self;
            systems = [ "cl-cc-vm" ];
          };
          default = cl-cc-vm;

          # Rendered documentation site (Material for MkDocs).
          # Build fully offline: Material for MkDocs bundles all of its assets,
          # so no network access is required inside the Nix sandbox. --strict
          # promotes broken links and unlisted pages to build failures.

          # sb-cover instrumentation is slow enough (tens of minutes) that it
          # does not belong in `checks` (every `nix flake check` would pay
          # that cost); `nix build .#coverage-report` runs it on demand, in
          # the same sandboxed HOME/TMPDIR the `default` check uses -- unlike
          # a bare `nix develop -c sbcl ...` invocation from an interactive
          # shell, which this repository's own refactoring sessions found
          # can stall indefinitely outside that sandbox for reasons specific
          # to the host shell, not to sb-cover itself.
          coverage-report =
            pkgs.runCommand "cl-cc-vm-coverage-report"
              {
                nativeBuildInputs = [
                  pkgs.sbcl
                  pkgs.coreutils
                ];
                CL_SOURCE_REGISTRY = sourceRegistry;
              }
              ''
                export HOME="$TMPDIR/home"
                mkdir -p "$HOME"
                cd "$TMPDIR"
                cp -r ${self} src-ro
                chmod -R u+w src-ro
                cd src-ro
                timeout 1800 sbcl --script run-coverage.lisp
                mkdir -p "$out"
                cp -r coverage-report/. "$out/"
              '';
        }
      );

      # `nix fmt` entry point.
      formatter = forAllSystems (system: treefmtEval.${system}.config.build.wrapper);

      # Granularity lives here, NOT in extra GitHub Actions jobs: `nix flake
      # check` evaluates each attribute as its own derivation, in parallel,
      # with build caching. Add a check here rather than a job in ci.yml.
      checks = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default =
            pkgs.runCommand "cl-cc-vm-tests"
              {
                nativeBuildInputs = [
                  pkgs.sbcl
                  pkgs.coreutils
                ];
                CL_SOURCE_REGISTRY = sourceRegistry;
              }
              ''
                export HOME="$TMPDIR/home"
                mkdir -p "$HOME" "$out"
                timeout 300 sbcl --script ${self}/run-tests.lisp
                touch "$out/passed"
              '';

          # Fails `nix flake check` when any tracked file is unformatted,
          # turning the formatter into an enforced CI gate.
          formatting = treefmtEval.${system}.config.build.check self;

          # Dead code, TODO/FIXME markers, and adapter/backward-compatibility
          # shims have been repeatedly verified absent by hand across this
          # repository's refactoring passes -- this turns that one-off
          # verification into a standing invariant CI enforces on every push,
          # matching the gate cl-process-kit and cl-log-kit already run.
          #
          # README.md's own prose about *not* having an adapter layer
          # necessarily contains the word "adapter"; that is why this reads
          # source and test files only, not every tracked file.
          noForbiddenMarkers =
            pkgs.runCommand "cl-cc-vm-no-forbidden-markers" { nativeBuildInputs = [ pkgs.gnugrep ]; }
              ''
                cd ${self}
                if grep -rniE 'TODO|FIXME|XXX|adapter|backward.?compat' \
                    --include='*.lisp' --include='*.asd' src t; then
                  echo "Forbidden marker found above -- this project keeps zero" >&2
                  echo "TODO/FIXME/XXX/adapter/backward-compat markers." >&2
                  exit 1
                fi
                touch "$out"
              '';

          # 500 is cl-weave's own stated org guideline (see its CHANGELOG's
          # "no source file exceeds 500 lines" entry), adopted verbatim
          # rather than inventing a different number -- a file crossing it
          # is exactly the moment `paredit refactor split-file`/`move-form`
          # should split it, the way this repository's own oversized files
          # already were in an earlier pass.
          maxFileLength = pkgs.runCommand "cl-cc-vm-max-file-length" { } ''
            cd ${self}
            over=0
            for f in $(find src t -name '*.lisp'); do
              lines=$(wc -l < "$f")
              if [ "$lines" -gt 500 ]; then
                echo "$f has $lines lines, over the 500-line guideline." >&2
                over=1
              fi
            done
            if [ "$over" -ne 0 ]; then
              exit 1
            fi
            touch "$out"
          '';
        }
      );

      apps = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          test = pkgs.writeShellApplication {
            name = "cl-cc-vm-test";
            runtimeInputs = [
              pkgs.sbcl
              pkgs.coreutils
            ];
            text = ''
              export CL_SOURCE_REGISTRY="${sourceRegistry}"
              exec timeout 300 sbcl --script ${self}/run-tests.lisp
            '';
          };
          # sb-cover instrumentation compiles src/ far more slowly than a
          # plain load, hence the longer budget than the plain test app.
          coverage = pkgs.writeShellApplication {
            name = "cl-cc-vm-coverage";
            runtimeInputs = [
              pkgs.sbcl
              pkgs.coreutils
            ];
            text = ''
              export CL_SOURCE_REGISTRY="${sourceRegistry}"
              exec timeout 1800 sbcl --script ${self}/run-coverage.lisp
            '';
          };
        in
        {
          default = {
            type = "app";
            program = "${test}/bin/cl-cc-vm-test";
          };
          test = {
            type = "app";
            program = "${test}/bin/cl-cc-vm-test";
          };
          coverage = {
            type = "app";
            program = "${coverage}/bin/cl-cc-vm-coverage";
          };
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.mkShell {
            packages = [ pkgs.sbcl ];
            CL_SOURCE_REGISTRY = sourceRegistry;
          };
        }
      );
    };
}
