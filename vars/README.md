# PipelineWorks

The `/vars` directory used to contain files related to the shared
Jenkins library. This used to make changes to the library veru difficult, as
any change would be tied to a branch and an MR of the `vmwathena_blockchain`
repository.

In order to solve the problem, we have created a separate repository,
[PipelineWorks](https://gitlab.eng.vmware.com/blockchain/pipelineworks),
that contains the shared library code.

With these changes,

 - Developers should no longer make changes to the `/vars` directory. Instead,
   they should do this in the PipelineWorks repository;
 - Whenever `vmwathena_blockchain` is built, it will automatically download the
   latest version of the `master` branch of PipelineWorks. If one wants to use
   an alternative version, they can specify a PipelineWorks branch through the
   `shared_lib_branch` parameter. In a future change, we are going to enable
   the pipeline to automatically use an alternative branch to `master` should
   a PipelineWorks branch exist with the same name as the `vmwathena_blockchain`
   branch in use;
 - Shared library code can now be tested automatically and independently of the
   main application
