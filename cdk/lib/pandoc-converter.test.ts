import { App } from "aws-cdk-lib";
import { Template } from "aws-cdk-lib/assertions";
import { PandocConverter } from "./pandoc-converter";

describe("The PandocConverter stack", () => {
  it("matches the snapshot", () => {
    const app = new App();
    const stack = new PandocConverter(app, "PandocConverter", { stack: "content-api", stage: "TEST" });
    const template = Template.fromStack(stack);
    expect(template.toJSON()).toMatchSnapshot();
  });
});
