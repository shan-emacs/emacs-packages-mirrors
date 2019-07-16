helm-catkin is a package providing an interface to catkin-tools `https://catkin-tools.readthedocs.io/en/latest/'.
It integrates with `helm' such that the config is shown in a helm dialog and can be customized
with actions.

Besides adjusting the config, you can build the ROS packages in the workspace in a colored build buffer.

All `helm-catkin' functions require a workspace defined. This is saved in a global Lisp
variable called `helm-catkin-workspace'. Easiest way is to specify a workspace is by calling
the interactive function `helm-catkin-set-workspace' which asks you to enter a path to your
workspace. This command can also be used to change between different workspaces.

Alternatively you can leave this variable at nil and use all `helm-catkin' functions on a
"per-buffer" basis. This means, the workspace is guessed for the buffer you are callling
the function from (only works if `helm-catkin-workspace' is nil though).

Quick overview of provided functionality:
`helm-catkin-set-workspace'  :: Sets the path to the helm-catkin workspace for all further helm-catkin commands
`helm-catkin'                :: Main command for showing, configuring and building in a helm window
`helm-catkin-build'          :: Build one, multiple or all packages in the current workspace
`helm-catkin-init'           :: Initializes the workspace and create a src/ folder if it doesn't exist
`helm-catkin-clean'          :: Clean the workspace (remove build/, devel/ and install/ folders)
`helm-catkin-config-show'    :: Shows the current config in a new buffer
`helm-catkin-config-open'    :: Opens the .catkin_tools/profiles/default/config.yaml file in a buffer
