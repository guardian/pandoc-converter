import { GuEc2App } from '@guardian/cdk';
import { AccessScope } from '@guardian/cdk/lib/constants';
import type { GuStackProps } from '@guardian/cdk/lib/constructs/core';
import { GuStack } from '@guardian/cdk/lib/constructs/core';
import type { App } from 'aws-cdk-lib';
import { InstanceClass, InstanceSize, InstanceType } from 'aws-cdk-lib/aws-ec2';

export class PandocConverter extends GuStack {
	converter: GuEc2App;

	constructor(scope: App, id: string, props: GuStackProps) {
		super(scope, id, props);

		this.converter = new GuEc2App(this, {
			access: {
				scope: AccessScope.PUBLIC,
			},
			app: 'pandoc-converter',
			applicationPort: 9000,
			imageRecipe: 'pandoc-converter-ubuntu-jammy-x86',
			instanceType: InstanceType.of(InstanceClass.T3, InstanceSize.NANO),
			monitoringConfiguration: { noMonitoring: true },
			scaling: {
				minimumInstances: 1,
			},
			userData: {
				distributable: {
					executionStatement: 'ls pandoc-converter.closure', // no nix yet, can't install or run here
					fileName: 'pandoc-converter.closure',
				},
			},
		});
	}
}
