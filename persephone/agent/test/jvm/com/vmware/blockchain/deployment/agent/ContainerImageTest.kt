/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent

import com.vmware.blockchain.deployment.agent.docker.DockerHttpClient
import net.jqwik.api.Arbitraries
import net.jqwik.api.Arbitrary
import net.jqwik.api.Combinators
import net.jqwik.api.ForAll
import net.jqwik.api.Property
import net.jqwik.api.Provide
import net.jqwik.api.Tuple
import org.assertj.core.api.Assertions
import org.junit.jupiter.api.Test

/**
 * Test that [ContainerImage] instance is created with expected component parts after a full image
 * name is submitted as input for parsing.
 */
class ContainerImageTest {

    /**
     * Provider of arbitrary Container registry names with shrinking support.
     */
    @Provide
    fun registries(): Arbitrary<String> {
        val maxSegments = 4
        val hostSegment = Arbitraries.strings()
                .ofMaxLength(63)
                .withCharRange('a', 'z')
                .withCharRange('0', '9')
        val hosts = hostSegment.list()
                .ofMinSize(0).ofMaxSize(maxSegments)
                .map { it.joinToString(separator = ".") }
        val ports = Arbitraries.integers().between(0, 65535)

        return Combinators.combine(hosts, ports).`as` { host, port ->
            when {
                host.isNotBlank() && port != 0 -> "$host:$port"
                host.isNotBlank() -> host
                else -> ""
            }
        }
    }

    /**
     * Provider of arbitrary Container repository name optionally prefixed with registry name, with
     * shrinking support.
     */
    @Provide
    fun repositories(): Arbitrary<Tuple.Tuple2<String, String>> {
        val maxSegmentLength = 10
        val repositorySegment = Arbitraries.strings()
                .ofMinLength(1).ofMaxLength(maxSegmentLength)
                .withCharRange('A', 'Z')
                .withCharRange('a', 'z')
                .withCharRange('0', '9')
        return registries().flatMap { registry ->
            val maxSegments = when {
                registry.isNotBlank() -> 3
                else -> 1
            }

            repositorySegment.list()
                    .ofMinSize(1).ofMaxSize(maxSegments)
                    .map { Tuple.of(registry, it.joinToString(separator = "/")) }
        }
    }

    /**
     * Provider of arbitrary Container image tag name with shrinking support.
     */
    @Provide
    fun tags(): Arbitrary<String> {
        val maxTagLength = 10
        return Arbitraries.strings().ofMaxLength(maxTagLength)
                .withCharRange('A', 'Z')
                .withCharRange('a', 'z')
                .withCharRange('0', '9')
    }

    /**
     * Test that fully-specified canonical image name is created (i.e. parsed) properly.
     */
    @Test
    fun simpleCanonicalImageName() {
        val registry = "registry:443"
        val repository = "root/collection/image"
        val tag = "tag"
        val image = ContainerImage.newContainerImage("$registry/$repository:$tag")

        Assertions.assertThat(image.registry).isEqualTo(registry)
        Assertions.assertThat(image.repository).isEqualTo(repository)
        Assertions.assertThat(image.tag).isEqualTo(tag)
    }

    /**
     * Test that canonical image name with registry name omitted is created (i.e. parsed) properly.
     */
    @Test
    fun omittedRegistryImageName() {
        val repository = "image"
        val tag = "tag"
        val image = ContainerImage.newContainerImage("$repository:$tag")

        Assertions.assertThat(image.registry)
                .isEqualTo(DockerHttpClient.DEFAULT_CONTAINER_REGISTRY_NAME)
        Assertions.assertThat(image.repository).isEqualTo(repository)
        Assertions.assertThat(image.tag).isEqualTo(tag)
    }

    /**
     * Test the invariant property that any valid container image names will be translated to
     * [ContainerImage] properly.
     */
    @Property
    fun canonicalImageName(
        @ForAll("repositories") repository: Tuple.Tuple2<String, String>,
        @ForAll("tags") tag: String
    ) {
        val repositoryName = repository.get1()?.takeIf { it.isNotBlank() }
                ?.let { "$it/${repository.get2()}" }
                ?: repository.get2()
        val name = when {
            tag.isNotBlank() -> "$repositoryName:$tag"
            else -> repositoryName
        }

        val image = ContainerImage.newContainerImage(name)

        // Verify container image structure is created correctly.
        if (tag.isBlank()) {
            Assertions.assertThat(image.tag).isEqualTo("latest")
        } else {
            Assertions.assertThat(image.tag).isEqualTo(tag)
        }
        if (repository.get1().isBlank()) {
            Assertions.assertThat(image.registry)
                    .isEqualTo(DockerHttpClient.DEFAULT_CONTAINER_REGISTRY_NAME)
        } else {
            Assertions.assertThat(image.registry).isEqualTo(repository.get1())
        }
        Assertions.assertThat(image.repository).isEqualTo(repository.get2())
    }
}
