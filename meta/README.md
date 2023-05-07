Developer Notes
===============

Release Checklist
-----------------

Perform the following tasks for every release:

  - Update version in devil.el.
  - Update copyright notice in devil.el.
  - Update copyright notice in LICENSE.md.
  - Update CHANGES.md.
  - Run checks:

    ```sh
    make checks
    ```

  - Commit changes:

    ```sh
    git status
    git add -p
    git commit
    git push
    ```

  - Tag the release:

    ```
    VERSION=

    git commit -em "Set version to $VERSION"
    git tag $VERSION -m "Devil $VERSION"
    git push origin main $VERSION
    ```
